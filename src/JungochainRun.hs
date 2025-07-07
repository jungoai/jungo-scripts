{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import           Control.Exception    (throwIO)
import           Control.Monad.Logger
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.List            ((!?))
import           Data.Maybe           (fromJust)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.Yaml            (decodeFileEither)
import           Relude
import           System.Directory     (copyFile, doesFileExist)
import           Text.Pretty.Simple   (pShow)
import           Turtle               hiding (e, o, x)

-------------------------------------------------------------------------------

main :: IO ()
main = runStdoutLoggingT $ do
  homepath <- fromJust <$> lookupEnv "HOME"
  logDebugN $ "homepath: " <> T.pack homepath
  args <- getArgs
  let
    confPath = args !? 0 `orElse` "jungochain.yaml"
    specPath = args !? 1 `orElse` "spec.json"

  mconf <- liftIO $ decodeFileEither confPath
  conf  <- liftIO $ either throwIO pure mconf
  logDebugP conf

  specExist <- liftIO $ doesFileExist specPath
  let
    defaultBasePath' = homepath </> ".jungochain"
    appEnv = mkAppEnv conf defaultBasePath' $ if specExist then Just specPath else Nothing
  logDebugP appEnv

  liftIO $ runApp appEnv $ do
    mkBasePath
    installSpec
    addKeysIfIsValidator
    logDebugN "initializatin done, running node"
    runNode

data Config = Config
  { version       :: Text
  , name          :: Text
  , path          :: Maybe FilePath
  , chain_type    :: ChainType
  , telemetry_url :: Maybe [Text]
  , boot_nodes    :: Maybe [Text]
  , validator     :: Maybe ConfigValidator
  , is_archive    :: Maybe Bool
  , is_rpc        :: Maybe Bool
  , network       :: ConfNetwork
  , logging       :: ConfLog
  }
  deriving (Show, Generic, FromJSON, ToJSON)

defIsArchive = False
defIsRpc = False

data ChainType = Devnet | Mainnet | Local
  deriving (Show, Generic, FromJSON, ToJSON)

data ConfigValidator = ConfigValidator
  { node_key      :: Text
  , secret_phrase :: Text
  , password      :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data ConfNetwork = ConfNetwork
  { port     :: Int
  , rpc_port :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data ConfLog = ConfLog
  { max_size :: Text -- e.g: 10m
  , max_file :: Int  -- e.g: 10
  }
  deriving (Show, Generic, FromJSON, ToJSON)

type App = ReaderT AppEnv (LoggingT IO)

runApp :: AppEnv -> App a -> IO a
runApp appEnv app = runStdoutLoggingT $ runReaderT app appEnv

data AppEnv = AppEnv
  { image        :: Text
  , nodeName     :: Text
  , basePath     :: Text
  , chainType    :: Text
  , specIn       :: Maybe FilePath'
  , specOut      :: Text
  , specRawOut   :: Text
  , logMaxSize   :: Text
  , logMaxFile   :: Text
  , appPort      :: Text
  , rpcPort      :: Text
  , telemetryUrl :: Maybe Text
  , validator    :: Maybe Validator
  , bootNodes    :: Maybe Text
  , isArchive    :: Bool
  , isRpc        :: Bool
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Validator = Validator
  { nodeKey      :: Text
  , secretPhrase :: Text
  , password     :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

mkAppEnv :: Config -> FilePath -> Maybe FilePath -> AppEnv
mkAppEnv conf defaultBasePath' specIn =
  AppEnv
    { image         = "ghcr.io/jungoai/jungochain:" <> conf.version
    , nodeName      = conf.name
    , basePath      = basepath
    , chainType     = chaintype
    , specIn        = T.pack <$> specIn
    , specOut       = basepath // chaintype <> "_spec.json"
    , specRawOut    = basepath // chaintype <> "_spec_raw.json"
    , logMaxSize    = conf.logging.max_size
    , logMaxFile    = showT conf.logging.max_file
    , appPort       = showT conf.network.port
    , rpcPort       = showT conf.network.rpc_port
    , telemetryUrl  = quoted . intercalateT "," <$> conf.telemetry_url
    , bootNodes     = quoted . intercalateT "," <$> conf.boot_nodes
    , validator     = toval <$> conf.validator
    , isArchive     = conf.is_archive `orElse` defIsArchive
    , isRpc         = conf.is_rpc `orElse` defIsRpc
    }
  where
    toval ConfigValidator {..} = Validator
      { nodeKey       = node_key
      , secretPhrase  = quoted secret_phrase
      , password      = password
      }

    chaintype = T.toLower $ showT conf.chain_type
    basepath  = T.pack (fromMaybe defaultBasePath' conf.path) // conf.name

runNode :: App ()
runNode = do
  AppEnv{..} <- ask
  commonArgs' <- commonArgs
  conditionalArgs' <- conditionalArgs
  let
    runArgs =
      [ "run"
      , "--name " <> "jungochain-" <> chainType <> "-" <> nodeName
      , "--network    host"
      , "-v " <> basePath <> ":" <> basePath
      , "--log-driver json-file"
      , "--log-opt    max-size=" <> logMaxSize
      , "--log-opt    max-file=" <> logMaxFile
      , "-d"
      , image
      ]
  docker $ runArgs ++ commonArgs' ++ conditionalArgs'

commonArgs :: App [Text]
commonArgs  = do
  AppEnv {..} <- ask
  pure
    [ "--base-path       " <> basePath
    , "--chain           " <> specRawOut
    , "--name            " <> nodeName
    , "--public-addr     " <> "/ip4/0.0.0.0/tcp/" <> appPort
    , "--port            " <> appPort
    , "--rpc-port        " <> rpcPort
    , "--rpc-methods          Safe"
    , "--rpc-cors             all"
    , "--rpc-max-connections  5000"
    ]

-- If it is validator add:      "--validator"
-- if it is archive add:        "--state-pruning archive"
-- if it is not boot node add:  "--bootnodes " <> bootNodes env
-- if it is rpc node add:       "--unsafe-rpc-external
conditionalArgs :: App [Text]
conditionalArgs = do
  appEnv <- ask
  let
    validatorArgs     = case appEnv.validator of
      Nothing               ->
                        [ "--unsafe-force-node-key-generation"
                        ]
      Just(Validator {..})  ->
                        [ "--validator"
                        , "--node-key " <> nodeKey
                        , "--password " <> password
                        ]
    archiveArgs       = guard appEnv.isArchive >>
                        [ "--state-pruning archive"
                        ]
    syncArgs          = guard (isJust appEnv.bootNodes) >>
                        [ "--bootnodes " <> fromJust appEnv.bootNodes
                        ]
    rpcArgs           = guard appEnv.isRpc >>
                        [ -- Not all RPC methods are safe to be exposed publicly.
                          -- Use an RPC proxy server to filter out dangerous methods. More details:
                          -- <https://docs.substrate.io/build/remote-procedure-calls/#public-rpc-interfaces>.
                          "--unsafe-rpc-external"
                        ]
    telemetryUrlArgs  = guard (isJust appEnv.telemetryUrl) >>
                        [ "--telemetry-url " <> fromJust appEnv.telemetryUrl
                        ]
  pure $ validatorArgs ++ archiveArgs ++ syncArgs ++ rpcArgs ++ telemetryUrlArgs

mkBasePath :: App ()
mkBasePath = do
  basePath <- asks basePath
  mktree $ T.unpack basePath

installSpec :: App ()
installSpec = do
  installSpec'
  buildSpecRaw

installSpec' :: App ()
installSpec' = do
  e@AppEnv {..} <- ask
  case specIn of
    Just specIn' -> liftIO $ copyFile' specIn' specOut
    Nothing -> docker $ runArgs e.basePath e.image
      ++
        [ "build-spec"
        , "--disable-default-bootnode"
        , "--chain " <> chainType
        , "> " <> specOut
        ]

buildSpecRaw :: App ()
buildSpecRaw = do
  e@AppEnv {..} <- ask
  docker $ runArgs e.basePath e.image
    ++
      [ "build-spec"
      , "--disable-default-bootnode"
      , "--chain " <> specOut
      , "--raw"
      , "> " <> specRawOut
      ]

addKeysIfIsValidator :: App ()
addKeysIfIsValidator = do
  e <- ask
  case e.validator of
    Just Validator {..} -> do
      addKey "Sr25519" "aura" secretPhrase password
      addKey "Ed25519" "gran" secretPhrase password
    _                   -> pure ()

addKey :: Text -> Text -> Text -> Text -> App ()
addKey scheme keyType secretPhrase password = do
  e <- ask
  docker $ runArgs e.basePath e.image
    ++
      [ "key"
      , "insert"
      , "--base-path   " <> e.basePath
      , "--chain       " <> e.specRawOut
      , "--suri        " <> secretPhrase
      , "--password    " <> password
      , "--scheme      " <> scheme
      , "--key-type    " <> keyType
      ]

runArgs :: Text -> Text -> [Text]
runArgs basePath image =
  [ "run"
  , "-v " <> basePath <> ":" <> basePath
  , image
  ]

-------------------------------------------------------------------------------
-- Utils

type FilePath' = Text

(//) :: FilePath' -> FilePath' -> FilePath'
(//) x y = T.pack $ T.unpack x Turtle.</> T.unpack y

_procs_ x y = procs x y empty

docker :: MonadIO io => [Text] -> io ()
docker = shells_ "docker"

shells_ x y = shells (x <> " " <> T.unwords y) empty

showT :: (Show a) => a -> Text
showT = T.pack . show

intercalateT x y = T.pack $ intercalate x $ T.unpack <$> y

orElse = flip fromMaybe

logDebugP x = logDebugN . TL.toStrict $ pShow x

quoted :: Text -> Text
quoted t = T.pack (show (T.unpack t))

copyFile' i o = copyFile (T.unpack i) (T.unpack o)
