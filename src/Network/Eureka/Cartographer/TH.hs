{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{- |
  Template haskell tools for compiling and incliding Elm files.
-}
module Network.Eureka.Cartographer.TH (
  addCartographerMetadata
) where

import Data.GraphViz (parseDotGraph, DotGraph)
import Data.Map (insert)
import Data.Text.Lazy (Text, pack)
import Language.Haskell.TH (Dec(FunD), Exp(LitE, LetE, VarE, RecUpdE,
  AppE), Lit(StringL), Q, runIO, newName, Clause(Clause), Pat(AsP, RecP,
  VarP), Body(NormalB))
import Network.Eureka (InstanceConfig(InstanceConfig, instanceMetadata))

readDot :: FilePath -> Q Exp
readDot filename = runIO $ do
  dotStr <- readFile filename
  let parsedGraph :: DotGraph Text
      parsedGraph = parseDotGraph (pack dotStr)
  -- | verify that the dot graph parses without error
  parsedGraph `seq` (return . LitE . StringL) dotStr


{- |
  This function takes a filename that contains a dot graph and produces
  a template haskell expression which describes a function of the type:
  @InstanceConfig -> InstanceConfig@. The purpose of the returned function
  is to insert the content of the dot graph into the appropriate Eureka
  metadata for this instance.

  usage: @$(addCartographerMetadata "cartography.dot") myInstanceConfig@
-}
addCartographerMetadata :: FilePath -> Q Exp
addCartographerMetadata filename = do
  dotStr <- readDot filename
  addMetadata <- newName "addMetadata"
  instanceConfig <- newName "instanceConfig"
  metadata <- newName "metadata"

  
  {- |
    Build and return a let expression that looks like this:

      let addMetadata instanceConfig@InstanceConfig {
                instanceMetadata = metadata
              } =
            instanceConfig {
                instanceMetadata = insert "cartography" dotStr metadata
              }
      in addMetadata
  -}

  return $ LetE [
      FunD addMetadata [Clause [AsP
          instanceConfig
          (RecP 'InstanceConfig [('instanceMetadata, VarP metadata)])
        ]
        (NormalB (
            RecUpdE (VarE instanceConfig) [('instanceMetadata,
                AppE
                  (AppE
                      (AppE (VarE 'insert) (LitE (StringL "cartography")))
                      dotStr
                    )
                  (VarE metadata)
              )]
          ))
        []
      ]
    ] (VarE addMetadata)


