#!/usr/bin/env stack
{- stack
  script
  --resolver lts-12.7
  --package text
  --package ansi-wl-pprint
  --package mtl
  --package exceptions
  --package lens
  --package optparse-applicative
  --package shelly
  --
  +RTS -s -RTS
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Prelude hiding (FilePath)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Data.Maybe (fromMaybe)
import Control.Monad.Reader
import Control.Monad.Catch (Exception, MonadThrow, throwM, catch)
import Data.Typeable (Typeable)
import Control.Lens
import Options.Applicative as OA
import Shelly.Lifted
default (T.Text)

--TODO make fontname a type
data AppCommand = InstallFontEntryCommand String Bool
                | ShowFontStatusCommand
  deriving (Show)

data InstallFontException = NonemptyDirException FilePath
  deriving (Typeable)
instance Show InstallFontException where
  show (NonemptyDirException fp) = "Font dir not empty - " ++ show fp
instance Exception InstallFontException

data FontEntry = FontEntry {
  _fontEntryName :: String,
  _fontEntryRelPath :: FilePath,
  _fontEntryLoadCmd :: Sh FilePath
  }

data FontPathRoot = FontPathRoot {
  _fontPathRootLocal ::  FilePath,
  _fontPathRootSystem :: Maybe FilePath
  } deriving (Show)

data AppConfig = AppConfig {
      _appConfigFontPaths :: FontPathRoot
    } deriving (Show)

data Options = Options {
  _optionsAppCommand :: AppCommand,
  _optionsAppConfig :: AppConfig
  } deriving (Show)

makeLenses ''FontEntry
makeLenses ''FontPathRoot
makeFields ''AppConfig
makeLenses ''Options

fontEntries :: [FontEntry]
fontEntries =
  [
    (FontEntry
      "source-code-pro"
      "adobe-fonts/source-code-pro"
      (gitCloneLoadCmd
       "https://github.com/adobe-fonts/source-code-pro.git"
       ["--branch", "release", "--depth", "1"])),
    (FontEntry
      "source-sans-pro"
      "adobe-fonts/source-sans-pro"
      (gitCloneLoadCmd
       "https://github.com/adobe-fonts/source-sans-pro"
       ["--branch", "release", "--depth", "1"]))
  ]

gitCloneLoadCmd :: T.Text -> [T.Text] -> Sh FilePath
gitCloneLoadCmd repo cloneOptions = do
  tmpPathRaw <- cmd "mktemp" "-d"
  let tmpPath = T.strip .head  $ T.words tmpPathRaw --unsafe head, but should be throwing anyway
  run_ "git" $ ["clone"] ++ cloneOptions ++ [repo, tmpPath]
  return $ fromText tmpPath

getDefaultConfig :: (MonadSh m) => m AppConfig
getDefaultConfig = do
  home <- liftSh $ get_env_text "HOME"
  let fontPath' =
        fromText home
        </> ".local/share/fonts"
  return AppConfig {
    _appConfigFontPaths = FontPathRoot {
        _fontPathRootLocal = fontPath',
        _fontPathRootSystem = Just "/usr/local/share/fonts"
        }}

printFontPathRoot :: (MonadReader e m,
                      HasFontPaths e FontPathRoot,
                      MonadIO m,
                      MonadSh m)
  => m ()
printFontPathRoot = do
  e <- ask
  liftIO $ do
    putStrLn "LOCAL ROOT"
    putStrLn . show . toTextIgnore $ e^.fontPaths^.fontPathRootLocal
    putStrLn "" >> putStrLn "SYSTEM ROOT"
    putStrLn
      $ fromMaybe "N/A"
      $ show . toTextIgnore <$> e^.fontPaths^.fontPathRootSystem

fontsInstallStatus :: (MonadReader e m,
                       HasFontPaths e FontPathRoot,
                       MonadIO m,
                       MonadSh m)
  => m ()
fontsInstallStatus = do
  tokensForFontEntries <- forM fontEntries $ (\fe -> do
    isLocalInstalled <- localFontEntryPath fe >>= fontEntryInstalled
    isSystemInstalled <- systemFontEntryPath fe
                         >>= mapM fontEntryInstalled  --traverse Maybe
    return [fe^.fontEntryName,
            show . toTextIgnore $ fe^.fontEntryRelPath,
            show isLocalInstalled,
            fromMaybe "N/A" $ show <$> isSystemInstalled])
  let tokenRows = headerTokens : tokensForFontEntries
  liftIO . putStrLn . show . PP.vsep
    $ (\tr -> PP.hsep
              $ zipWith (\t i -> PP.fill i t) (PP.text <$> tr) colLens)
    <$> tokenRows
  where
    colLens = [20, 40, 16, 16] :: ([Int])
    headerTokens = ["NAME", "FONTPATH", "LOCAL INSTALLED", "SYSTEM INSTALLED"]

installFontEntry :: (MonadReader e m,
                     HasFontPaths e FontPathRoot,
                     MonadIO m,
                     MonadSh m)
  => FontEntry -> m ()
installFontEntry fontEntry = do
  tmpPath <- liftSh $ fontEntry^.fontEntryLoadCmd
  localPath <- localFontEntryPath fontEntry
  liftIO -- IO implements MonadCatch, so we catch in IO and lift it
    $ catch (shelly $ copyFontEntryToPath tmpPath localPath)
    (\nde@(NonemptyDirException _) ->
       putStrLn ("Failed to install " ++ fontEntry^.fontEntryName ++ " to local")
       >> (putStrLn . show $ nde))
  systemPath <- systemFontEntryPath fontEntry
  case systemPath of
    Nothing -> return ()
    Just sp -> liftIO
               $ catch (shelly $ copyFontEntryToPath tmpPath sp)
               (\nde@(NonemptyDirException _) ->
                  putStrLn ("Failed to install " ++ fontEntry^.fontEntryName ++ " to system")
                 >> (putStrLn . show $ nde))
  rm_rf tmpPath

uninstallFontEntry :: (MonadReader e m,
                       HasFontPaths e FontPathRoot,
                       MonadIO m,
                       MonadSh m)
  => FontEntry -> m ()
uninstallFontEntry fontEntry = do
  localPath <- localFontEntryPath fontEntry
  isLocalInstalled <- fontEntryInstalled localPath
  when isLocalInstalled $ rm_rf localPath
  systemPath <- systemFontEntryPath fontEntry
  case systemPath of
    Nothing -> return ()
    Just sp -> do
      isSystemInstalled <- fontEntryInstalled sp
      when isSystemInstalled $ rm_rf sp

localFontEntryPath :: (MonadReader e m, HasFontPaths e FontPathRoot)
  => FontEntry -> m FilePath
localFontEntryPath fontEntry =
  ask >>= (\e -> return $ fontEntryPath (e^.fontPaths^.fontPathRootLocal) fontEntry)

systemFontEntryPath :: (MonadReader e m, HasFontPaths e FontPathRoot)
  => FontEntry -> m (Maybe FilePath)
systemFontEntryPath fontEntry =
  ask >>= (\e -> return $ fontEntryPath
                 <$> (e^.fontPaths^.fontPathRootSystem)
                 <*> return fontEntry)

fontEntryPath :: FilePath -> FontEntry -> FilePath
fontEntryPath installFilePathRoot fontEntry =
  installFilePathRoot </> (fontEntry^.fontEntryRelPath)

copyFontEntryToPath :: (MonadSh m, MonadThrow m) => FilePath -> FilePath -> m ()
copyFontEntryToPath sourceFilePath installFilePath = do
  isInstalled <- fontEntryInstalled installFilePath
  when isInstalled $ throwM $ NonemptyDirException installFilePath
  liftSh $ cp_r sourceFilePath installFilePath

--TODO could write better?
fontEntryInstalled :: (MonadSh m) => FilePath ->  m Bool
fontEntryInstalled installFilePath = do
  exists <- test_d installFilePath
  if exists
    then (ls installFilePath >>= (return . not . Prelude.null))
    else return False

appCommandParser :: Parser AppCommand
appCommandParser =
  hsubparser (OA.command "status" statusParserInfo
              <> OA.command "install" installParserInfo)
  where
    statusParserInfo = info (pure ShowFontStatusCommand) mempty
    installParserInfo =
      info (InstallFontEntryCommand
             <$> OA.argument str (help "Font name" <> metavar "FONT NAME")
             <*> switch (short 'u' <> help "Uninstall"))
      mempty

-- MonadReader m only saves from passing in a starting FontPaths, not much gained
fontPathsParser :: (MonadReader e m, HasFontPaths e FontPathRoot)
  => m (Parser FontPathRoot)
fontPathsParser = do
  e <- ask
  let localPathParser =
        strOption
        (long "localpath"
         <> help "Local font path"
         <> showDefault
         <> value (e^.fontPaths^.fontPathRootLocal)
         <> metavar "LOCAL FONT PATH")
      systemPathParser =
        optional $ strOption -- optional creates Parser (Just -) if not empty, else f Nothing
        (long "syspath"
         <> help "System font path"
         <> showDefault
         <> fromMaybe mempty (value <$> (e^.fontPaths^.fontPathRootSystem))
         <> metavar "SYSTEM FONT PATH")
      noSystemPathParser = flag' Nothing
        (long "nosyspath"
         <> help "No system font path")
  return $ FontPathRoot
    <$> localPathParser
    <*> (systemPathParser <|> noSystemPathParser)

opts :: (MonadReader e m, HasFontPaths e FontPathRoot)
  => m (ParserInfo Options)
opts = do
  fpParser <- fontPathsParser
  return $ info
    ((Options
      <$> appCommandParser
      <*> (AppConfig <$> fpParser))
      <**> helper)
    (fullDesc
      <> progDesc "installs fonts from the shell, with way too many Haskell dependencies"
      <> header "shelly font installer - a dumb font installer")

lookupFontEntry :: String -> Maybe FontEntry
lookupFontEntry lookupFontName =
  L.find (\fe -> fe^.fontEntryName == lookupFontName) fontEntries

go :: (MonadReader AppConfig m,  MonadIO m, MonadSh m) => m ()
go = do
  options <- opts >>= (liftIO . execParser)
  local (const $ options^.optionsAppConfig) $ do
    (case (options^.optionsAppCommand) of
       InstallFontEntryCommand feName uninstall ->
         (case lookupFontEntry feName of
            Just fe -> if uninstall then uninstallFontEntry fe
                       else installFontEntry fe
            Nothing -> liftIO . putStrLn $ feName ++ " not found")
       ShowFontStatusCommand ->
         printFontPathRoot >> (liftIO $ putStrLn "") >> fontsInstallStatus)

main :: IO ()
main = do
  shelly $ verbosely $ do
    defaultAppConfig <- getDefaultConfig
    runReaderT go defaultAppConfig
