-----------------------------------------------------------------------------
--
-- Module      :  GenImplicit
-- Copyright   :  (c) hokum
-- License     :  GPL3
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

import Data.Aeson
import Control.Applicative
import Control.Monad

import qualified Control.Monad.RWS.Lazy as RWS_
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8
--import Network.HTTP.Conduit (simpleHttp)

import qualified Graphics.Implicit as I
import qualified Graphics.Implicit.Primitives as IP

import qualified Cmd_arguments as CmdA
import qualified System.Environment as SE

import Generation_branch
import Genimplicit_types
import Feathers



{----}
-- Read the local copy of the JSON file.
getJSON :: FilePath -> IO B.ByteString
getJSON f = B.readFile f
--}

{-
-- Read the remote copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL
--}--}


main :: IO ()
main = do
 a<- SE.getArgs
 routine a


routine :: [String] -> IO ()
routine args
   |feathers_branch_only = return ()
   |otherwise = output_stl_default
   where

   output_stl_default = do
      d <- (eitherDecode <$> getJSON jif) :: IO (Either String BlenderData)
      case d of
        Left err -> putStrLn err
        Right bo -> do
            let (a, w) = RWS_.evalRWS (render bo) () gs
            mapM B8.putStrLn w
            I.writeSTL mq stl_ef a

   output_feathers = do
      d <- (eitherDecode <$> getJSON jif) :: IO (Either String BlenderDataFeathers)
      case d of
        Left err -> putStrLn err
        Right bo -> do
            let (a, w) = RWS_.evalRWS (grow_feathers bo) () (Feather_settings {})
            mapM B8.putStrLn w
            B.putStrLn $ encode a


 --     where
   mq = (\(CmdA.InputArguments {CmdA.mesh_quality = (Just d)}) -> d) inputArgs'
   jif = (\(CmdA.InputArguments {CmdA.json_import_file = (Just d)}) -> d) inputArgs'
   jef = (\(CmdA.InputArguments {CmdA.json_export_file = (Just d)}) -> d) inputArgs'
   stl_ef = (\(CmdA.InputArguments {CmdA.stl_export_file = (Just d)}) -> d) inputArgs'
   gs = (Generation_settings
       {
        overall_union_rounding = (\(CmdA.InputArguments
            {CmdA.overall_union_rounding = (Just d)}
            ) -> d) inputArgs'
       })
   feathers_branch_only = (\(CmdA.InputArguments {CmdA.just_feathers = (Just d)}) -> d) inputArgs'

   inputArgs' = CmdA.inputArgs $ CmdA.tag_DMap args


