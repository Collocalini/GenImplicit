-----------------------------------------------------------------------------
--
-- Module      :  Genimplicit_types
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
{-# LANGUAGE DeriveGeneric #-}


module Genimplicit_types (
   BlenderObject (..)
  ,BlenderData (..)
  ,Generation_settings (..)
) where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics


-- | Type of each JSON entry in record syntax.
data BlenderObject =
  BlenderObject { name  :: !T.Text
                 ,type_ :: !T.Text
                 ,group :: ![T.Text]
                 ,rounding :: Float
                 ,x     :: Float
                 ,y     :: Float
                 ,z     :: Float
                 ,dim_x     :: Float
                 ,dim_y     :: Float
                 ,dim_z     :: Float
                 ,scale_x     :: Float
                 ,scale_y     :: Float
                 ,scale_z     :: Float
                 ,rot_x     :: Float
                 ,rot_y     :: Float
                 ,rot_z     :: Float
                 ,rot_w     :: Float
                } deriving (Show,Generic)

data BlenderData =
   BlenderData { objects :: [BlenderObject]
                ,groups  :: ![T.Text]
               } deriving (Show,Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON BlenderObject
instance ToJSON BlenderObject

instance FromJSON BlenderData
instance ToJSON BlenderData





data Generation_settings = Generation_settings
   {
    overall_union_rounding :: Float
   }



