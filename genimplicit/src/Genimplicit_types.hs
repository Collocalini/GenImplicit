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
  ,BlenderFeatherBase (..)
  ,BlenderFeather (..)
  ,BlenderData (..)
  ,BlenderDataFeathers (..)
  ,BlenderExportFeathers (..)
  ,Generation_settings (..)
  ,Feather_settings (..)

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




data BlenderFeatherBase =
  BlenderFeatherBase { fb_name  :: !T.Text
                 ,fb_type_ :: !T.Text
                 ,fb_group :: ![T.Text]
                 ,feathers_number :: Int
                 ,fb_x     :: Float
                 ,fb_y     :: Float
                 ,fb_z     :: Float
                 ,fb_dim_x     :: Float
                 ,fb_dim_y     :: Float
                 ,fb_dim_z     :: Float
                 ,fb_scale_x     :: Float
                 ,fb_scale_y     :: Float
                 ,fb_scale_z     :: Float
                 ,fb_rot_x     :: Float
                 ,fb_rot_y     :: Float
                 ,fb_rot_z     :: Float
               --  ,fb_rot_w     :: Float
                 ,fb_growth_dir_x     :: Float
                 ,fb_growth_dir_y     :: Float
                 ,fb_growth_dir_z     :: Float
               --  ,fb_growth_dir_w     :: Float
                } deriving (Show,Generic)


data BlenderFeather =
  BlenderFeather {
                  ftype_ :: !T.Text
                 ,fgroup :: ![T.Text]
                 ,fx     :: Float
                 ,fy     :: Float
                 ,fz     :: Float
                 ,fdim_x     :: Float
                 ,fdim_y     :: Float
                 ,fdim_z     :: Float
                 ,frot_x     :: Float
                 ,frot_y     :: Float
                 ,frot_z     :: Float
                 ,frot_w     :: Float
                } deriving (Show,Generic)



data BlenderData =
   BlenderData { objects :: [BlenderObject]
                ,groups  :: ![T.Text]
               } deriving (Show,Generic)

data BlenderDataFeathers =
   BlenderDataFeathers
               { fobjects :: [BlenderFeatherBase]
                ,fgroups  :: ![T.Text]
               } deriving (Show,Generic)


data BlenderExportFeathers =
   BlenderExportFeathers { feathers :: [BlenderFeather]} deriving (Show,Generic)




-- Instances to convert our type to/from JSON.

instance FromJSON BlenderObject
instance ToJSON BlenderObject

instance FromJSON BlenderData
instance ToJSON BlenderData

instance FromJSON BlenderFeatherBase
instance ToJSON BlenderFeatherBase

instance FromJSON BlenderFeather
instance ToJSON BlenderFeather

instance FromJSON BlenderDataFeathers
instance ToJSON BlenderDataFeathers

instance FromJSON BlenderExportFeathers
instance ToJSON BlenderExportFeathers

data Generation_settings = Generation_settings
   {
    overall_union_rounding :: Float
   }



data Feather_settings = Feather_settings
   {

   }


