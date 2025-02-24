{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Control.Exception (throwIO)
import Data.Aeson (FromJSON)
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Data.Yaml (decodeFileEither)
import GHC.Generics (Generic)
import Turtle hiding (x)

main :: IO ()
main = do
    vars <- env
    let homePath = getHomePath vars
    debug "homePath" homePath

    res <- decodeFileEither (homePath </> ".jungochain.yaml")
    ymlConf <- either throwIO return res :: IO YmlConfig
    debug "ymlConf" ymlConf
    let
        defaultPath = homePath </> ".jungochain"
        conf = ymlConfToAppConf defaultPath ymlConf
    debug "conf" conf

    mktree $ T.unpack $ basePath conf
    exportRawSpec conf
    when (isValidator conf) $ do addKeysToKeystore conf
    runNode conf $ commonArgs conf ++ conditionalArgs conf

data YmlConfig = YmlConfig
    { node :: YmlConfNode
    , network :: YmlConfNetwork
    , logging :: YmlConfLog
    }
    deriving (Show, Generic, FromJSON)

{- FOURMOLU_DISABLE -}
data YmlConfNode = YmlConfNode
    { version       :: Text
    , name          :: Text
    , key           :: Maybe Text
    , path          :: Maybe FilePath
    , chain_type    :: ChainType
    , secret_phrase :: Maybe Text
    , password      :: Maybe Text
    , telemetry_url :: Maybe [Text]
    , is_boot_node  :: Maybe Bool
    , boot_nodes    :: [Text]
    , is_validator  :: Bool
    , is_archive    :: Bool
    , is_rpc        :: Bool
    }
    deriving (Show, Generic, FromJSON)
{- FOURMOLU_ENABLE -}

data ChainType = Devnet | Mainnet | Local
    deriving (Show, Generic, FromJSON)

data YmlConfNetwork = YmlConfNetwork
    { port :: Int
    , rpc_port :: Int
    }
    deriving (Show, Generic, FromJSON)

data YmlConfLog = YmlConfLog
    { max_size :: Text -- e.g: 10m
    , max_file :: Int -- e.g: 10
    }
    deriving (Show, Generic, FromJSON)

data AppConfig = AppConfig
    { image :: Text
    , nodeName :: Text
    , nodeKey :: Maybe Text
    , basePath :: Text
    , chainType :: Text
    , spec :: Text
    , specRaw :: Text
    , logMaxSize :: Text
    , logMaxFile :: Text
    , appPort :: Text
    , rpcPort :: Text
    , telemetryUrl :: Text
    , isValidator :: Bool
    , secretPhrase :: Maybe Text -- Needed if isValidator Ture
    , appPassword :: Maybe Text -- Needed if isValidator True
    , isBootNode :: Bool
    , bootNodes :: Text
    , isArchive :: Bool
    , isRpc :: Bool
    }
    deriving (Show, Generic, FromJSON)

ymlConfToAppConf :: FilePath -> YmlConfig -> AppConfig
ymlConfToAppConf defaultPath yc =
    AppConfig
        { image = "ghcr.io/jungoai/jungochain:" <> (version . node) yc
        , nodeName = nodeName_
        , nodeKey = key $ node yc
        , basePath = basePath_ & T.pack
        , chainType = chainType_
        , spec = basePath_ </> T.unpack (chainType_ <> "_spec.json") & T.pack
        , specRaw = basePath_ </> T.unpack (chainType_ <> "_spec_raw.json") & T.pack
        , logMaxSize = max_size $ logging yc
        , logMaxFile = showT $ max_file $ logging yc
        , appPort = showT $ port $ network yc
        , rpcPort = showT $ rpc_port $ network yc
        , telemetryUrl = intercalateT "," $ fromMaybe [] $ telemetry_url $ node yc
        , isBootNode = fromMaybe False $ is_boot_node $ node yc
        , bootNodes = intercalateT "," $ boot_nodes $ node yc
        , isValidator = is_validator $ node yc
        , secretPhrase = secret_phrase $ node yc
        , appPassword = password $ node yc
        , isArchive = is_archive $ node yc
        , isRpc = is_rpc $ node yc
        }
    where
        nodeName_ = name $ node yc
        chainType_ = T.toLower $ showT $ chain_type $ node yc
        basePath_ = basePath__ </> T.unpack nodeName_

        basePath__ = fromMaybe defaultPath $ path $ node yc

runNode c args = shells_ "docker" $ runJungochainImage ++ args
    where
        runJungochainImage =
            [ "run"
            , "--name " <> "jungochain-" <> chainType c
            , "--network    host"
            , "-v " <> basePath c <> ":" <> basePath c
            , "--log-driver json-file"
            , "--log-opt    max-size=" <> logMaxSize c
            , "--log-opt    max-file=" <> logMaxFile c
            , "-d"
            , image c
            ]

-- If is validator add:     "--validator"
-- if is archive add:       "--state-pruning archive"
-- if is not boot node add: "--bootnodes " <> bootNodes env
commonArgs c =
    [ "--base-path       " <> basePath c
    , "--chain           " <> specRaw c
    , "--name            " <> nodeName c
    , "--public-addr     " <> "/ip4/0.0.0.0/tcp/" <> appPort c
    , "--port            " <> appPort c
    , "--rpc-port        " <> rpcPort c
    , "--rpc-methods          Safe"
    , "--rpc-cors             all"
    , "--rpc-max-connections  5000"
    ]

conditionalArgs c =
    validatorArgs ++ archiveArgs ++ syncArgs ++ rpcArgs ++ telemetryUrlArgs
    where
        validatorArgs
            | isValidator c =
                [ "--validator"
                , "--node-key " <> fromJust (nodeKey c)
                , "--password " <> fromJust (appPassword c)
                ]
            | otherwise =
                [ "--unsafe-force-node-key-generation"
                ]
        archiveArgs = guard (isArchive c) >> ["--state-pruning archive"]
        syncArgs = guard (not (isBootNode c)) >> ["--bootnodes " <> bootNodes c]
        rpcArgs =
            guard (not (isBootNode c))
                >> [
                     -- Not all RPC methods are safe to be exposed publicly.
                     -- Use an RPC proxy server to filter out dangerous methods. More details:
                     -- <https://docs.substrate.io/build/remote-procedure-calls/#public-rpc-interfaces>.
                     "--unsafe-rpc-external"
                   ]
        telemetryUrlArgs =
            guard ((not . T.null) (telemetryUrl c))
                >> ["--telemetry-url \"" <> telemetryUrl c <> "\""]

exportRawSpec c = do
    exportSpec c
    specToRaw c

exportSpec c =
    shells_
        "docker"
        $ runImage c
            ++ [ "build-spec"
               , "--disable-default-bootnode"
               , "--chain " <> chainType c
               , "> " <> spec c
               ]

specToRaw c =
    shells_
        "docker"
        $ runImage c
            ++ [ "build-spec"
               , "--disable-default-bootnode"
               , "--chain " <> chainType c
               , "--raw"
               , "> " <> specRaw c
               ]

addKeysToKeystore c = do
    addKey "Sr25519" "aura" c
    addKey "Ed25519" "gran" c

addKey scheme keyType c =
    shells_
        "docker"
        $ runImage c
            ++ [ "key"
               , "insert"
               , "--base-path   " <> basePath c
               , "--chain       " <> specRaw c
               , "--suri        " <> fromJust (secretPhrase c)
               , "--password    " <> fromJust (appPassword c)
               , "--scheme      " <> scheme
               , "--key-type    " <> keyType
               ]

runImage c =
    [ "run"
    , "-v " <> basePath c <> ":" <> basePath c
    , image c
    ]

-------------------------------------------------------------------------------
-- Utils

_procs_ x y = procs x y empty

shells_ x y = shells (x <> " " <> T.unwords y) empty

showT :: (Show a) => a -> Text
showT = T.pack . show

intercalateT x y = T.pack $ intercalate x $ T.unpack <$> y

getHomePath :: [(Text, Text)] -> FilePath
getHomePath [] = error "HOME environment not set"
getHomePath (x : xs)
    | fst x == "HOME" = T.unpack $ snd x
    | otherwise = getHomePath xs

debug :: (Show a) => String -> a -> IO ()
debug title x = putStrLn ("[Debug]: " <> title <> ": " <> show x)
