-----------------------------------------------------------------------------
--
-- Module      :  Feathers
-- Copyright   :  (c) hokum
-- License     :  GPL-3
--
-- Maintainer  :
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Feathers (
grow_feathers
) where


import qualified Control.Monad.State.Lazy as S
import qualified Control.Monad.Writer.Lazy as W
import qualified Control.Monad.RWS.Lazy as RWS_
import qualified Data.ByteString.Char8 as B
import qualified Data.List as L
import qualified Data.Matrix as DM
import qualified Data.Text as T
import qualified Data.Maybe as M

import Genimplicit_types



type Feather_branch = RWS_.RWS () [B.ByteString] Feather_settings BlenderExportFeathers
type Log = W.Writer [B.ByteString]



grow_feathers :: BlenderDataFeathers -> Feather_branch
grow_feathers df = return $ BlenderExportFeathers { feathers = []}









places_to_grow_feathers :: BlenderFeatherBase -> DM.Matrix (Float, Float, Float)
places_to_grow_feathers (BlenderFeatherBase {
                  fb_name = n
                 ,fb_type_ = t
                 ,fb_group = g
                 ,feathers_number = fn
                 ,fb_x = x
                 ,fb_y = y
                 ,fb_z = z
                 ,fb_dim_x = dx
                 ,fb_dim_y = dy
                 ,fb_dim_z = dz
              {- ,fb_scale_x = sx
                 ,fb_scale_y = sy
                 ,fb_scale_z = sz -}
                 ,fb_rot_x = rx
                 ,fb_rot_y = ry
                 ,fb_rot_z = rz
               --  ,fb_rot_w = rw
                 ,fb_growth_dir_x = gdx
                 ,fb_growth_dir_y = gdy
                 ,fb_growth_dir_z = gdz
               --  ,fb_growth_dir_w = gdw
                }) = DM.matrix 1 1 (\(x,y)-> (0,0,0))








