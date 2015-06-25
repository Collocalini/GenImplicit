-----------------------------------------------------------------------------
--
-- Module      :  Cmd_arguments
-- Copyright   :
-- License     :  PublicDomain
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Cmd_arguments (

flags ,

options,

tag_DMap,

list_arguments,

InputArguments(..),
inputArgs,
) where

import qualified Data.Map as DMap


data InputArguments = InputArguments {
    json_import_file :: Maybe FilePath
   ,stl_export_file  :: Maybe FilePath
   ,mesh_quality     :: Maybe Float
   ,overall_union_rounding :: Maybe Float
   }


{-- ================================================================================================
================================================================================================ --}
inputArgs :: DMap.Map String String -> InputArguments
inputArgs tm = InputArguments {
   json_import_file = argument   argument_json_import_file   default_json_import_file
  ,stl_export_file  = argument   argument_stl_export_file    default_stl_export_file
  ,mesh_quality     = float_argument   argument_mesh_quality    default_mesh_quality
  ,overall_union_rounding = float_argument   argument_overall_union_rounding   default_overall_union_rounding
  }
  where
  file_argument :: String -> String -> Maybe FilePath
  file_argument argument_name default_value = argument argument_name default_value

  float_argument :: String -> String -> Maybe Float
  float_argument argument_name default_value = try_read $ argument argument_name default_value
    where
    try_read Nothing = Nothing
    try_read (Just "") = Nothing
    try_read (Just s)  = Just $ read s

  argument :: String -> String -> Maybe String
  argument argument_name default_value
   -- |s/= default_value = Just s
    |s== "" = Nothing
    |otherwise = Just s
    where
    s = (DMap.findWithDefault default_value argument_name tm)




argument_json_import_file = "json-import-file"
default_json_import_file  = ""

argument_stl_export_file  = "stl-export-file"
default_stl_export_file   = ""

argument_mesh_quality     = "mesh-quality"
default_mesh_quality      = "1"

argument_overall_union_rounding = "overall-union-rounding"
default_overall_union_rounding  = "0"

flags = [

        ]

options =  [
            argument_json_import_file
           ,argument_stl_export_file
           ,argument_mesh_quality
           ,argument_overall_union_rounding
           ]

{-- ================================================================================================
================================================================================================ --}
tag_DMap:: [String] -> DMap.Map String String
tag_DMap [] = DMap.fromList [
        --("",""),
    (argument_json_import_file,       default_json_import_file)
   ,(argument_stl_export_file,        default_stl_export_file)
   ,(argument_mesh_quality,           default_mesh_quality)
   ,(argument_overall_union_rounding, default_overall_union_rounding)
   ]----]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]]

tag_DMap lst = DMap.union (DMap.fromList $ map (\(Just x) -> x) $ list_arguments lst) $
                                                                                       tag_DMap []
----------------------------------------------------------------------


{-- ================================================================================================
================================================================================================ --}
list_arguments :: [String] -> [Maybe (String, String)]
list_arguments [] = []
list_arguments (tag:rest)
  | take 2 tag == "--" && elem tag' flags =
                       (Just (tag', "true")) : list_arguments rest
  | take 2 tag == "--" && elem tag' options =
                       (Just (tag', after_tag)) : list_arguments rest'

  |otherwise = list_arguments rest

  where
     after_tag = head rest
     tag' = (drop 2 tag)

     rest'
        |rest /= [] = tail rest
        |otherwise = []
     rest''
        |rest' /= [] = tail rest'
        |otherwise = []
----------------------------------------------------

