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
import qualified Data.Text as T

import qualified Control.Monad.RWS.Lazy as RWS_
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8
--import Network.HTTP.Conduit (simpleHttp)

import qualified Graphics.Implicit as I
import qualified Graphics.Implicit.Primitives as IP

import qualified Cmd_arguments as CmdA
import qualified System.Environment as SE

import Generation_branch
import qualified ESCAD_branch as ESCADb
import Genimplicit_types




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
   |otherwise = output_escad_default
   where

   output_stl_default = do
      d <- (eitherDecode <$> getJSON jif) :: IO (Either String BlenderData)
      case d of
        Left err -> putStrLn err
        Right bo -> do
            let (a, w) = RWS_.evalRWS (render bo) () gs
            mapM B8.putStrLn w
            I.writeSTL mq stl_ef a

      where
        mq = (\(CmdA.InputArguments {CmdA.mesh_quality = (Just d)}) -> d) inputArgs'
        jif = (\(CmdA.InputArguments {CmdA.json_import_file = (Just d)}) -> d) inputArgs'
        stl_ef = (\(CmdA.InputArguments {CmdA.stl_export_file = (Just d)}) -> d) inputArgs'
        gs = (Generation_settings
           {
            overall_union_rounding = (\(CmdA.InputArguments
                {CmdA.overall_union_rounding = (Just d)}
                ) -> d) inputArgs'
           })
           
   output_escad_default = do
      d <- (eitherDecode <$> getJSON jif) :: IO (Either String BlenderData)
      escad <- (return . T.pack) =<< readFile eif
      case d of
        Left err -> putStrLn err
        Right bo -> do
            let (a, w) = RWS_.evalRWS (ESCADb.render escad bo) () gs
            mapM B8.putStrLn w
            writeFile escad_ef $ T.unpack a

      where
        --mq = (\(CmdA.InputArguments {CmdA.mesh_quality = (Just d)}) -> d) inputArgs'
        jif = (\(CmdA.InputArguments {CmdA.json_import_file = (Just d)}) -> d) inputArgs'
        eif = (\(CmdA.InputArguments {CmdA.escad_import_file = (Just d)}) -> d) inputArgs'
        escad_ef = (\(CmdA.InputArguments {CmdA.escad_export_file = (Just d)}) -> d) inputArgs'
        gs = (Generation_settings
           {
            overall_union_rounding = (\(CmdA.InputArguments
                {CmdA.overall_union_rounding = (Just d)}
                ) -> d) inputArgs'
           })

   inputArgs' = CmdA.inputArgs $ CmdA.tag_DMap args


