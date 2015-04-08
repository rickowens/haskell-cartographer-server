{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
{- |
  This program scans a Eureka instances for services and tried to build and/or
  render a DOT graph of the relationships between the services, based on
  metadata provided by the services themselves.
-}
module Main (main) where

import Prelude hiding (readFile, putStr)

import Canteven.Config (canteven)
import Control.Applicative ((<|>))
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.Version (showVersion)
import Network.Eureka (InstanceConfig(instanceAppName,
  instanceNonSecurePort), def)
import Network.Eureka.Cartographer.HTTP (Config(Config, eureka), withEureka, website)
import Paths_cartographer_server (version)
import Snap (Snap, pass, getsRequest, rqPathInfo)
import Web.Moonshine (runMoonshine, setBanner, setServerVersion, liftSnap, notFound)


main :: IO ()
main = do
  setBanner banner
  Config {eureka} <- canteven
  withEureka eureka instanceConfig (\econn ->
      runMoonshine $ do
        setServerVersion "cartographer" version
        liftSnap (
            matchPathInfo "" (website econn)
            <|> notFound
          )
    )


{- |
  The eureka instance configuration for this process.
-}
instanceConfig :: InstanceConfig
instanceConfig = def {
    instanceAppName = "cartographer",
    instanceNonSecurePort = 8080
    -- ^ TODO: Maybe don't hard-code this.
  }


{- |
  Executes the snap action if the request uri matches the uri argument, or else
  passes on the next alternative.
-}
matchPathInfo :: ByteString -> Snap () -> Snap ()
matchPathInfo uri snap =
  bool pass snap . (uri ==) =<< getsRequest rqPathInfo


{- |
  Something pretty.
-}
banner :: String
banner = "\n\n\
  \|<><><><><><><><><><><><><><><><><><><><><><><>|   \n\
  \|                      *                       |   \n\
  \| cartographer         |                       |   \n\
  \|                     -|-                      |\n"
  ++ versionLine ++
  "|                      *                       |\n\
  \|<><><><><><><><><><><><><><><><><><><><><><><>|\n\n\n"
  where
    versionLine =
      "| version: "
        ++ showVersion version
        ++ replicate (12 - length (showVersion version)) ' '
        ++ "|                       |\n"

