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
                 ,rounding :: Double
                 ,x     :: Double
                 ,y     :: Double
                 ,z     :: Double
                 ,dim_x     :: Double
                 ,dim_y     :: Double
                 ,dim_z     :: Double
                 ,scale_x     :: Double
                 ,scale_y     :: Double
                 ,scale_z     :: Double
                 ,rot_x     :: Double
                 ,rot_y     :: Double
                 ,rot_z     :: Double
                 ,rot_w     :: Double
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
    overall_union_rounding :: Double
   }
