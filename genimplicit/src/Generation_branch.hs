-----------------------------------------------------------------------------
--
-- Module      :  Generation_branch
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
{-# LANGUAGE OverloadedStrings #-}

module Generation_branch (
 render
--,spread_by_groupes
--,render_groups_together
--,render_groups_togetherR
--,process_all_groups



) where

import qualified Control.Monad.State.Lazy as S
import qualified Control.Monad.Writer.Lazy as W
import qualified Control.Monad.RWS.Lazy as RWS_
import qualified Data.ByteString.Char8 as B
import qualified Graphics.Implicit as I
import qualified Graphics.Implicit.Primitives as IP
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Maybe as M
import qualified Text.ParserCombinators.Parsec as P

import Genimplicit_types








type SymbolicObj3_Generation_branch = RWS_.RWS () [B.ByteString] Generation_settings I.SymbolicObj3
type Log = W.Writer [B.ByteString]





render :: BlenderData -> SymbolicObj3_Generation_branch
render blender_data@(
   BlenderData {objects= bos
              ,groups = groups
              })
   = do
      s@(Generation_settings {overall_union_rounding=rounding}) <- RWS_.get

      case rounding == 0.0 of
         True ->  render_groups_together $ spread_by_groupes groups bos
         False -> render_groups_togetherR $ spread_by_groupes groups bos



spread_by_groupes :: [T.Text] -> [BlenderObject] -> [(T.Text, [BlenderObject])]
spread_by_groupes groupes b = filter filter_case' $
   ("", objects_that_are_not_in_any_group b):
       (
       map (\g-> (g, filter (object_is_in_specified_group g) b) ) $
          filter filter_case'' groupes
       )
   where
   groups_object_is_in :: BlenderObject -> [T.Text]
   groups_object_is_in (BlenderObject {group=g'}) = filter filter_case'' g'

   object_is_in_specified_group :: T.Text -> BlenderObject -> Bool
   object_is_in_specified_group g b = elem g $ groups_object_is_in b

   objects_that_are_not_in_any_group :: [BlenderObject] -> [BlenderObject]
   objects_that_are_not_in_any_group b =
      map (\(b1,_)-> b1) $ filter filter_case $ zip b $ map groups_object_is_in b

   filter_case (_, [])  = True
   filter_case (_, _ )  = False



   filter_case' (_, [])  = False
   filter_case' (_, _ )  = True


   filter_case'' group_name =
     case (P.parse (group_name_parser) "(unknown)" $ T.unpack group_name) of
          Left e -> False
          Right r -> True



render_groups_together :: [(T.Text, [BlenderObject])] -> SymbolicObj3_Generation_branch
render_groups_together groups = do
   let (a,w) = W.runWriter $ process_all_groups groups
   RWS_.tell w
   return $ I.union a


render_groups_togetherR :: [(T.Text, [BlenderObject])] -> SymbolicObj3_Generation_branch
render_groups_togetherR groups = do
   (Generation_settings {overall_union_rounding=rounding}) <- RWS_.get
   let (a,w) = W.runWriter $ process_all_groups groups
   RWS_.tell w
   return $ I.unionR rounding a




process_all_groups :: [(T.Text, [BlenderObject])] ->
                      Log [I.SymbolicObj3]
process_all_groups groups = mapM (process_group) $ groups


process_group :: (T.Text, [BlenderObject]) -> Log I.SymbolicObj3
process_group (group_name, b) = do
   m <- make_objects b
   --W.tell w
   return $ I.unionR rounding m
   where
   rounding = (\(Group_settings {group_rounding=gr})-> gr) $ parse_group_name group_name






data Group_settings =
     Group_settings {group_rounding :: Float}
    |Radius Float
    |Rubbish
    deriving (Show)


group_settings = Group_settings {group_rounding=0}

give_rounding_radius :: [Group_settings] -> Float
give_rounding_radius g = (\(Radius x)-> x) $ M.fromMaybe (Radius 0) $ L.find is_radius g
   where
   is_radius :: Group_settings -> Bool
   is_radius (Radius _) = True
   is_radius _          = False



parse_group_name :: T.Text -> Group_settings
parse_group_name "" = group_settings
parse_group_name group_name =
   case (P.parse (group_name_parser) "(unknown)" $ T.unpack group_name) of
      Left e -> group_settings
      Right r -> r



group_name_parser :: P.Parser Group_settings
group_name_parser = do
   P.manyTill P.anyChar (P.string $ "p:")
   P.try P.spaces
   p<- P.sepEndBy try_properties (P.spaces)
   return $ Group_settings {group_rounding = give_rounding_radius p}


try_properties =
      (P.choice [rounding_radius_option,
                 do P.many1 $ P.choice [P.many1 P.alphaNum
                                       ,P.many1 (P.oneOf ":;=,.")]
                    return Rubbish])



rounding_radius_option :: P.Parser Group_settings
rounding_radius_option = do
   P.string $ "r="
   rl<- P.manyTill P.digit (P.char '.')
   rr<- P.manyTill P.digit (P.eof)
   return $ Radius $ read $ rl ++ "." ++ rr



make_objects :: [BlenderObject] -> Log [I.SymbolicObj3]
make_objects [] = return []
make_objects bos = do
   m <- mapM make_object bos
   return $ M.catMaybes m


make_object :: BlenderObject -> Log (Maybe I.SymbolicObj3)
make_object b@(BlenderObject {type_="Cube"})     = make_cube b
make_object b@(BlenderObject {type_="rect3d"})   = make_cube b
make_object b@(BlenderObject {type_="Sphere"})   = make_sphere b
make_object b@(BlenderObject {type_="Cylinder"}) = make_cylinder b
make_object b@(BlenderObject {type_="cube"})     = make_cube b
make_object b@(BlenderObject {type_="sphere"})   = make_sphere b
make_object b@(BlenderObject {type_="cylinder"}) = make_cylinder b
make_object b@(BlenderObject {type_= _ })        = make_sphere b



make_cube :: BlenderObject -> Log (Maybe I.SymbolicObj3)
make_cube b@(BlenderObject {  x =x
                             ,y =y
                             ,z =z
                             ,dim_x=dx
                             ,dim_y=dy
                             ,dim_z=dz
                             ,scale_x=sx
                             ,scale_y=sy
                             ,scale_z=sz
                             ,rot_x=rx
                             ,rot_y=ry
                             ,rot_z=rz
                             ,rot_w=rw
                             ,rounding=rnd
                             })
   | input_is_allright = return $ Just $ I.translate (x, y, z) $ IP.rotate3V (rw)  (rx,  ry, rz) $
                             I.rect3R rnd bottom_left top_right
   |otherwise = do W.tell ["Invalid argument in make_cube: " , (B.pack $ show b)]
                   return Nothing
   where
   bottom_left = (- dx/2, - dy/2, - dz/2)
   top_right   = (  dx/2,   dy/2,   dz/2)
   input_is_allright = (sx >= 0) && (sy >= 0) && (sz >= 0)


make_sphere :: BlenderObject -> Log (Maybe I.SymbolicObj3)
make_sphere b@(BlenderObject {x =x
                             ,y =y
                             ,z =z
                             ,scale_x=sx
                             ,scale_y=sy
                             ,scale_z=sz
                             ,rot_x=rx
                             ,rot_y=ry
                             ,rot_z=rz
                             ,rot_w=rw
                             })
   | input_is_allright = return $ Just $ I.translate (x, y, z) $ IP.rotate3V (rw)  (rx,  ry, rz) $
                             I.scale (sx, sy, sz) $ I.sphere 1
   |otherwise = do W.tell ["Invalid argument in make_sphere: " , (B.pack $ show b)]
                   return Nothing
   where
      input_is_allright = (sx >= 0) && (sy >= 0) && (sz >= 0)


make_cylinder :: BlenderObject -> Log (Maybe I.SymbolicObj3)
make_cylinder b@(BlenderObject {x =x
                               ,y =y
                               ,z =z
                               ,scale_x=sx
                               ,scale_y=sy
                               ,scale_z=sz
                               ,rot_x=rx
                               ,rot_y=ry
                               ,rot_z=rz
                               ,rot_w=rw
                               })
   | input_is_allright = return $ Just $ I.translate (x, y, z) $ IP.rotate3V (rw)  (rx,  ry, rz) $
                           I.scale (sx, sy, sz) $ I.cylinder 1 2
   |otherwise = do W.tell ["Invalid argument in make_cylinder: " , (B.pack $ show b)]
                   return Nothing
   where
      input_is_allright = (sx >= 0) && (sy >= 0) && (sz >= 0)






















