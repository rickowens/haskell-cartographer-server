{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
  This is where all the real code for the cartographer lives.
-}
module Network.Eureka.Cartographer (
  Config(..),
  withEureka,
  website
) where

import Prelude hiding (lookup)

import Control.Exception (try, evaluate, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(parseJSON), Value(Object), (.:),
  Value(String))
import Data.ByteString (hGetContents)
import Data.Char (toLower)
import Data.GraphViz (DotGraph(DotGraph, strictGraph, directedGraph,
  graphID, graphStatements), DotStatements(DotStmts, attrStmts, subGraphs,
  nodeStmts, edgeStmts), ParseDotRepr, parseDotGraph, GraphvizOutput(Svg),
  GraphvizCommand(Dot), graphvizWithHandle, DotNode(DotNode, nodeID,
  nodeAttributes))
import Data.List.Split (splitOn)
import Data.Map (insert, elems, lookup, keys)
import Data.Maybe (fromMaybe)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Network.Eureka (EurekaConfig(eurekaServerServiceUrls),
  DataCenterInfo(DataCenterMyOwn, DataCenterAmazon), EurekaConnection,
  InstanceConfig(InstanceConfig, instanceMetadata),
  def, discoverDataCenterAmazon, lookupAllApplications,
  InstanceInfo(InstanceInfo, instanceInfoMetadata))
import Network.Eureka.Cartographer.TH (readDot)
import Network.HTTP.Client (withManager, defaultManagerSettings)
import Snap (Snap, writeBS, modifyResponse, setHeader, getParam)
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)
import qualified Data.Map as Map (fromList)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Lazy as TL (pack)
import qualified Network.Eureka as E (withEureka)

data Config = Config {
    eureka :: CartographerEurekaConfig
  } deriving (Generic)

instance FromJSON Config

data CartographerEurekaConfig
  = EurekaDeveloper {
        serviceUrls :: [String] 
      }
  | EurekaAmazon {
        serviceUrls :: [String] 
      } deriving Show

instance FromJSON CartographerEurekaConfig where
  parseJSON (Object v) = do
    datacenter <- v .: "datacenterType"
    (String urls) <- v .: "serviceUrls"
    return $ constructor datacenter (splitOn "," $ T.unpack urls)
    where
      constructor :: Text -> [String] -> CartographerEurekaConfig
      constructor "developer" = EurekaDeveloper
      constructor "amazon" = EurekaAmazon
      constructor s = error (
          "bad value for datacenterType config parameter: " ++ show s ++
          ". Valid values are 'developer' and 'amazon'."
        )

  parseJSON v = error $ "couldn't parse Eureka Config from: " ++ show v


{- |
  Our own version of `withEureka`, which delegates to
  `Network.Eureka.withEureka`
-}
withEureka
  :: CartographerEurekaConfig
  -> InstanceConfig
  -> (EurekaConnection -> IO a)
  -> IO a
withEureka EurekaAmazon {serviceUrls} instanceConfig a = do
  dataCenter <- withManager defaultManagerSettings discoverDataCenterAmazon
  eurekaWithServiceUrls
    serviceUrls
    instanceConfig
    (DataCenterAmazon dataCenter)
    a
withEureka EurekaDeveloper {serviceUrls} instanceConfig a =
  eurekaWithServiceUrls
    serviceUrls
    instanceConfig
    DataCenterMyOwn
    a


{- |
  Helper for `withEureka`. Adds cartography metadata and delegates to
  `E.withEureka`.
-}
eurekaWithServiceUrls
  :: [String]
  -> InstanceConfig
  -> DataCenterInfo
  -> (EurekaConnection -> IO a)
  -> IO a
eurekaWithServiceUrls urls = E.withEureka eurekaConfig . addMetadata
  where
    {- |
      Adds the cartography metadata to the instance config.
    -}
    addMetadata :: InstanceConfig -> InstanceConfig
    addMetadata conf@InstanceConfig {instanceMetadata} =
      conf {
          instanceMetadata =
            insert "cartography" $(readDot) instanceMetadata
      }

    eurekaConfig = def {
        eurekaServerServiceUrls = Map.fromList [("default", urls)]
      }


{- |
  The actual snap website that generates the pretty pictures.
-}
website :: EurekaConnection -> Snap ()
website eConn = do
  apps <- liftIO (lookupAllApplications eConn)
  (serveDotGraph . addNodes (keys apps) . joinInstances . concat . elems) apps

  where
    emptyGraph :: DotGraphT
    emptyGraph = DotGraph {
        strictGraph = True,
        directedGraph = True,
        graphID = Nothing,
        graphStatements = DotStmts {
          attrStmts = [],
          subGraphs = [],
          nodeStmts = [],
          edgeStmts = []
        }
      }


    addNodes :: [String] -> DotGraphT -> DotGraphT
    addNodes nodes graph@DotGraph {graphStatements} =
      graph {
          graphStatements = foldr addNode graphStatements nodes
        }

    addNode :: String -> DotStatementsT -> DotStatementsT
    addNode node stmts@DotStmts {nodeStmts} = stmts {
        nodeStmts =
          let nodeId = TL.pack (fmap toLower node) in
          DotNode {nodeID = nodeId, nodeAttributes = []} : nodeStmts
      }

    joinInstances :: [InstanceInfo] -> DotGraphT
    joinInstances = foldr joinInstance emptyGraph

    joinInstance :: InstanceInfo -> DotGraphT -> DotGraphT
    joinInstance inst dot =
      joinGraph dot (instToGraph inst)

    joinGraph :: DotGraphT -> DotGraphT -> DotGraphT
    joinGraph
        graph@DotGraph {graphStatements = s1}
        DotGraph {graphStatements = s2} =
      graph {graphStatements = joinStatements s1 s2}

    joinStatements :: DotStatementsT -> DotStatementsT -> DotStatementsT
    joinStatements s1 s2 = DotStmts {
        attrStmts = attrStmts s1 ++ attrStmts s2,
        subGraphs = subGraphs s1 ++ subGraphs s2,
        nodeStmts = nodeStmts s1 ++ nodeStmts s2,
        edgeStmts = edgeStmts s1 ++ edgeStmts s2
      }

    instToGraph :: InstanceInfo -> DotGraphT
    instToGraph InstanceInfo {instanceInfoMetadata} =
      maybe emptyGraph parseGraph (lookup "cartography" instanceInfoMetadata)

    parseGraph :: String -> DotGraphT
    parseGraph dotStr =
      case parseDotGraphEither (TL.pack dotStr) of
        Left _ -> emptyGraph
        Right g -> g

    serveDotGraph :: DotGraphT -> Snap ()
    serveDotGraph graph = do
      graphType <- getGraphType
      modifyResponse (setHeader "Content-Type" "image/svg+xml")
      writeBS =<< liftIO (graphvizWithHandle graphType graph Svg hGetContents)

{- |
  Figures out the graph type based on the query string param "graphType".
-}
getGraphType :: Snap GraphvizCommand
getGraphType = do
  typeM <- getParam "graphType"
  return $ case typeM of
    Nothing -> Dot
    Just str ->
      fromMaybe Dot (readMaybe (T.unpack (decodeUtf8 str)))


{- |
  Shorthand type for dot graphs.
-}
type DotGraphT = DotGraph Text


{- |
  Shorthand type for graph statements
-}
type DotStatementsT = DotStatements Text


{- |
  Parse a dot graph using Either. Sadly, `parseDotGraph` in the graphviz
  package is not total and throws a value error on bad input. Since
  we can only catch the error in the IO monad, we are forced to use
  `unsafePerformIO` to make this function total.  This is safe because
  it doesn't matter when, or how many times, this gets executed, and
  there are no external side effects.
-}
parseDotGraphEither :: (ParseDotRepr dg n) => Text -> Either String (dg n)
parseDotGraphEither dotStr = unsafePerformIO $ do
  graphE <- try (evaluate (parseDotGraph dotStr))
  return $ case graphE of
    Left e -> Left (show (e :: SomeException))
    Right graph -> Right graph


