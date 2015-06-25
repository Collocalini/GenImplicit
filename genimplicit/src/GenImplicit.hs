{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Aeson
import qualified Data.List as L
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State.Lazy as S
import qualified Control.Monad.Writer.Lazy as W
import qualified Data.ByteString.Lazy as B
--import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics

import qualified Graphics.Implicit as I
import qualified Graphics.Implicit.Primitives as IP

import qualified Cmd_arguments as CmdA
import qualified System.Environment as SE

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
   |otherwise = output_stl_default
   where

   output_stl_default = do
      d <- (eitherDecode <$> getJSON jif) :: IO (Either String BlenderData)
      case d of
        Left err -> putStrLn err
        Right bo ->
            I.writeSTL mq stl_ef $ S.evalState (render' bo) gs

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

   inputArgs' = CmdA.inputArgs $ CmdA.tag_DMap args



spread_by_groupes :: [T.Text] -> [BlenderObject] -> [(T.Text, [BlenderObject])]
spread_by_groupes groupes b = filter filter_case' $
   ("", objects_that_are_not_in_any_group b):
   map (\g-> (g, filter (object_is_in_specified_group g) b) ) groupes
   where
   groups_object_is_in :: BlenderObject -> [T.Text]
   groups_object_is_in (BlenderObject {group=g'}) = g'

   object_is_in_specified_group :: T.Text -> BlenderObject -> Bool
   object_is_in_specified_group g b = elem g $ groups_object_is_in b

   objects_that_are_not_in_any_group :: [BlenderObject] -> [BlenderObject]
   objects_that_are_not_in_any_group b =
      map (\(b1,_)-> b1) $ filter filter_case $ zip b $ map groups_object_is_in b

   filter_case (_, [])  = True
   filter_case (_, _ )  = False

   filter_case' (_, [])  = False
   filter_case' (_, _ )  = True



render_debug :: BlenderData -> String
render_debug blender_data@(
  BlenderData {objects= bos
              ,groups = groups
              })
   = unlines $ map (show) $ spread_by_groupes groups bos


render' :: BlenderData -> S.State Generation_settings I.SymbolicObj3
render' blender_data@(
  BlenderData {objects= bos
              ,groups = groups
              })
   = do
      (Generation_settings {overall_union_rounding=rounding}) <- S.get

      case rounding == 0.0 of
         True ->  return $ render_groups_together $ spread_by_groupes groups bos
         False -> render_groups_togetherR $ spread_by_groupes groups bos






data Generation_settings = Generation_settings
   {
    overall_union_rounding :: Float
   }



render_groups_together :: [(T.Text, [BlenderObject])] -> I.SymbolicObj3
render_groups_together groups = I.union $ process_all_groups groups


render_groups_togetherR :: [(T.Text, [BlenderObject])] -> S.State Generation_settings I.SymbolicObj3
render_groups_togetherR groups = do
   (Generation_settings {overall_union_rounding=rounding}) <- S.get
   return $ I.unionR rounding $ process_all_groups groups



process_all_groups :: [(T.Text, [BlenderObject])] ->
                      [I.SymbolicObj3]
process_all_groups groups = map (process_group) $ groups





process_group :: (T.Text, [BlenderObject]) -> I.SymbolicObj3
process_group (group_name, b) = I.unionR rounding $ make_objects b
   where
   rounding = (\(Group_settings {group_rounding=gr})-> gr) $ parse_group_name group_name






data Group_settings =
   Group_settings {group_rounding :: Float} deriving (Show)



parse_group_name :: T.Text -> Group_settings
parse_group_name "" = Group_settings {group_rounding=0}
parse_group_name group_name =
   Group_settings {group_rounding = read $ L.drop 2 $ (L.last.L.words.T.unpack) group_name}


make_objects :: [BlenderObject] -> [I.SymbolicObj3]
make_objects bos = map make_object bos


make_object :: BlenderObject -> I.SymbolicObj3
make_object b@(BlenderObject {type_="Cube"})     = make_cube b
make_object b@(BlenderObject {type_="rect3d"})   = make_cube b
make_object b@(BlenderObject {type_="Sphere"})   = make_sphere b
make_object b@(BlenderObject {type_="Cylinder"}) = make_cylinder b
make_object b@(BlenderObject {type_="cube"})     = make_cube b
make_object b@(BlenderObject {type_="sphere"})   = make_sphere b
make_object b@(BlenderObject {type_="cylinder"}) = make_cylinder b
make_object b@(BlenderObject {type_= _ })        = make_sphere b



make_cube :: BlenderObject -> I.SymbolicObj3
make_cube (BlenderObject {  x =x
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
                         }) =
   I.translate (x, y, z) $ IP.rotate3V (rw)  (rx,  ry, rz) $
      I.rect3R rnd bottom_left top_right
   where
   bottom_left = (- dx/2, - dy/2, - dz/2)
   top_right   = (  dx/2,   dy/2,   dz/2)



make_sphere :: BlenderObject -> I.SymbolicObj3
make_sphere (BlenderObject {x =x
                           ,y =y
                           ,z =z
                           ,scale_x=sx
                           ,scale_y=sy
                           ,scale_z=sz
                           ,rot_x=rx
                           ,rot_y=ry
                           ,rot_z=rz
                           ,rot_w=rw
                         }) =
   I.translate (x, y, z) $ IP.rotate3V (rw)  (rx,  ry, rz) $ I.scale (sx, sy, sz) $
      I.sphere 1



make_cylinder :: BlenderObject -> I.SymbolicObj3
make_cylinder (BlenderObject {x =x
                             ,y =y
                             ,z =z
                             ,scale_x=sx
                             ,scale_y=sy
                             ,scale_z=sz
                             ,rot_x=rx
                             ,rot_y=ry
                             ,rot_z=rz
                             ,rot_w=rw
                             }) =
   I.translate (x, y, z) $ IP.rotate3V (rw)  (rx,  ry, rz) $ I.scale (sx, sy, sz) $
      I.cylinder 1 2






