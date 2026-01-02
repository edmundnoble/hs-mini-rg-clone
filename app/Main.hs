
module Main (main) where

import Prelude hiding (lines)

import Chronos
import Control.Exception
import Control.Lens hiding (argument)
import Control.Monad
import Data.Containers.ListUtils
import Data.IORef
import Data.HashMap.Strict(HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable
import Data.Primitive.PrimVar
import Data.Text (Text)
import Data.Text qualified as StrictText
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy.IO qualified as Text
import Data.Text.Foreign qualified as Text
import Options.Applicative
import System.Directory.OsPath.Streaming
import System.File.OsPath
import System.IO (IOMode(..))
import System.OsPath
import System.OsString qualified as OsString
import Text.Regex.Base
import Text.Regex.TDFA ()
import Text.Regex.TDFA.Text qualified as RegexTDFA
import Torsor(scale)
import UnliftIO.Async
import System.Directory.OsPath

-- this can be done more efficiently using some of the internals of
-- directory-ospath-streaming, but it's not that interesting for performance
-- compared to the regex part
getDirectoryContentsRecursiveAbs fp = do
    over (mapped . _1) (fp </>) <$> getDirectoryContentsRecursive fp

main :: IO ()
main = do
    join $ execParser (info parser mempty)
    where
    parser = do
        par <- switch
            (long "par" <> help "Use parallelism")
        shouldPrint <- switch
            (long "print" <> help "Print each line")
        regex <- argument str
            (metavar "REGEX")
        inputFiles <- some (argument (OsString.unsafeEncodeUtf <$> str)
            (metavar "FILEORDIR"))
        return $ go par shouldPrint regex inputFiles

    go par shouldPrint regex inputFiles = do
        compiledRegex :: RegexTDFA.Regex <-
            makeRegexM (regex :: Text)
        filesVar :: PrimVar _ Int <- newPrimVar 0
        linesVar :: PrimVar _ Int <- newPrimVar 0
        matchesVar :: PrimVar _ Int <- newPrimVar 0
        bytesVar :: PrimVar _ Int <- newPrimVar 0
        charsVar :: PrimVar _ Int <- newPrimVar 0
        errorsRef :: IORef [OsPath] <- newIORef mempty
        timeTaken <- stopwatch_ $ do
            allFiles <- nubOrdOn fst . concat <$>
                traverse getDirectoryContentsRecursiveAbs inputFiles
            (if par then forConcurrently_ else forM_) allFiles $ \(filePath, fileType) -> do
                -- excludes symlinks and dirs
                when (fileType == File Regular) $ do
                    relFilePath <- makeRelativeToCurrentDirectory filePath
                    withFile relFilePath ReadMode $ \h -> do
                        handleIOError errorsRef filePath $ do
                            fileContents <- Text.hGetContents h
                            let fileLines = Text.lines fileContents
                            let add v i = void $ fetchAddInt v i
                            add filesVar 1
                            forM_ fileLines $ \line -> do
                                add linesVar 1
                                -- just iterate once over the linked structure
                                let Lengths {wordLen, charLen} = Text.foldlChunks
                                        (\acc t -> acc <> Lengths (Text.lengthWord8 t) (fromIntegral $ StrictText.length t))
                                        mempty line
                                add bytesVar wordLen
                                add charsVar charLen
                                when (match compiledRegex line) $ do
                                    when shouldPrint $
                                        Text.putStrLn line
                                    add matchesVar 1

        statsFiles <- readPrimVar filesVar
        statsLines <- readPrimVar linesVar
        statsMatches <- readPrimVar matchesVar
        statsBytes <- readPrimVar bytesVar
        statsChars <- readPrimVar charsVar
        statsErrors <- readIORef errorsRef

        putStrLn $ "files: " <> show statsFiles
        putStrLn $ "lines: " <> show statsLines
        putStrLn $ "matches: " <> show statsMatches
        putStrLn $ "bytes: " <> show statsBytes
        putStrLn $ "chars: " <> show statsChars
        putStrLn $ "errors: " <>
            show statsErrors
        putStrLn $ "speed (MB/s): " <>
            show (fromIntegral statsBytes `div` asSeconds (scale 1000000 timeTaken))

data Lengths = Lengths { wordLen :: !Int, charLen :: Int }
instance Semigroup Lengths where
    Lengths w c <> Lengths w' c' = Lengths (w + w') (c + c')
instance Monoid Lengths where
    mempty = Lengths 0 0

handleIOError errorsRef hashedFilePath act = do
    try @IOException act >>= \case
        Left _ -> atomicModifyIORef' errorsRef $ \allErrors ->
            (hashedFilePath : allErrors, ())
        Right _ -> return ()
