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

module ESCAD_branch (
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
import qualified Data.Map as DMap
import qualified Text.ParserCombinators.Parsec as P

import Genimplicit_types
import Generation_branch hiding (render,spread_by_groupes)
import qualified Generation_branch as GB


type Generation_branch a = RWS_.RWS () 
                                          [B.ByteString] 
                                          Generation_settings 
                                          a
                                          
type MappedTextObj3_Generation_branch = Generation_branch
                                          (DMap.Map T.Text T.Text)
type TextObj3_Generation_branch = Generation_branch T.Text  
                                              
{-                                   
type MappedTextObj3_Generation_branch = RWS_.RWS () 
                                          [B.ByteString] 
                                          Generation_settings 
                                          (DMap.Map T.Text T.Text)
type TextObj3_Generation_branch = RWS_.RWS () 
                                          [B.ByteString] 
                                          Generation_settings 
                                          T.Text


data ApplicationLog =
    OtherError       String
   |Note             String
   |Dump             String
   |ApplicationLogEmpty
   deriving (Show)
-}

render :: T.Text -> BlenderData -> TextObj3_Generation_branch
render  escad
        blender_data@(
   BlenderData {objects= bos
              ,groups = groups
              })
   = do 
      rgt <- render_groups_together $ spread_by_groupes groups bos
      parsed_escad <- parse_escad escad
      rp <- replacePlaceholders 
                parsed_escad
                rgt 
      return $ eSCAD_To_Text rp
      
      
      
spread_by_groupes :: [T.Text] -> [BlenderObject] -> [(T.Text, [BlenderObject])]
spread_by_groupes groupes b = filter filter_case' $
   ("", objects_that_are_not_in_any_group b):
       (
       map (\g-> (g, filter (object_is_in_specified_group g) b) ) $
          groupes
       )
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






replacePlaceholders :: ESCAD 
                    -> DMap.Map T.Text T.Text 
                    -> Generation_branch ESCAD
replacePlaceholders e@(ESCAD_Text t) _ = do 
  RWS_.tell [B.pack $ T.unpack $ T.append "replacePlaceholders: ESCAD_Text" t]
  return e
  
replacePlaceholders (ESCAD_Group g) m = do
  let gt = M.fromMaybe "/* not found */" $ DMap.lookup g m
  RWS_.tell [B.pack $ T.unpack 
    $ T.concat ["replacePlaceholders: ESCAD_Group ", g, " ", gt]]
  return $ ESCAD_Text gt 
  
replacePlaceholders (ESCAD_List l) m = do 
  RWS_.tell ["replacePlaceholders: ESCAD_List"]
  (return . ESCAD_List) =<<
    mapM (\e-> replacePlaceholders e m) l

isESCAD_Text (ESCAD_Text _) = True
isESCAD_Text _ = False

eSCAD_To_Text (ESCAD_Text t) = t
eSCAD_To_Text (ESCAD_Group _) = ""
eSCAD_To_Text (ESCAD_List l) = T.concat $ map eSCAD_To_Text l


render_groups_together :: [(T.Text, [BlenderObject])] 
                       -> MappedTextObj3_Generation_branch
render_groups_together groups = do
  let (a,w) = W.runWriter $ process_all_groups groups
  RWS_.tell w
  RWS_.tell [B.concat ["render_groups_together length=", 
                       B.pack $ show $ length a]]
  RWS_.tell $ map (\(l,r)-> B.concat ["group=", B.pack $ T.unpack l,"; ", 
                                      "object=", B.pack $ T.unpack r]) a
  return 
    $ foldl (\m (g,t)-> DMap.insertWith 
                          (\old new->T.concat [old,";\n",new]) 
                          g 
                          t 
                          m) 
            DMap.empty 
            a


process_all_groups :: [(T.Text, [BlenderObject])] ->
                      Log [(T.Text, T.Text)]
process_all_groups groups = mapM (process_group) $ groups


process_group :: (T.Text, [BlenderObject]) -> Log (T.Text, T.Text)
process_group (group_name, b) = do 
  mo<- make_objects b
  W.tell [B.concat ["process_group ", B.pack $ T.unpack group_name]]
  return (group_name, T.unlines mo)



data ESCAD =
     ESCAD_Text T.Text
    |ESCAD_Group T.Text
    |ESCAD_List [ESCAD]
    deriving (Show)


escadDefault = ESCAD_Text ""

parse_escad :: T.Text -> Generation_branch ESCAD
parse_escad "" = do 
                    RWS_.tell ["parse_escad: ESCAD was empty"]
                    return escadDefault
parse_escad escadContent =
  case (P.parse (escad_parser) "(unknown)" $ T.unpack escadContent) of
      Left e -> do 
        RWS_.tell [B.pack $ show e]
        return escadDefault
      Right r -> return r



escad_parser :: P.Parser ESCAD
escad_parser = do
   l<- P.many1 $ P.try makeoutGroup
   last<- P.many P.anyChar
   return $ ESCAD_List $ concat ( l ++ [[ESCAD_Text $ T.pack last]])



makeoutGroup :: P.Parser [ESCAD]
makeoutGroup = do
   t<- P.manyTill P.anyChar (P.string $ "${")
   p<- P.manyTill P.anyChar (P.string $ "}")
   return $ (ESCAD_Text $ T.pack $ t):[ESCAD_Group $ T.pack $ p]






make_objects :: [BlenderObject] -> Log [T.Text]
make_objects [] = return []
make_objects bos = do
   m <- mapM make_object bos
   return $ M.catMaybes m


make_object :: BlenderObject -> Log (Maybe T.Text)
make_object b@(BlenderObject {type_="Cube"})     = make_cube b
make_object b@(BlenderObject {type_="rect3d"})   = make_cube b
make_object b@(BlenderObject {type_="Sphere"})   = make_sphere b
make_object b@(BlenderObject {type_="Cylinder"}) = make_cylinder b
make_object b@(BlenderObject {type_="Cone"})     = make_cone b
make_object b@(BlenderObject {type_="cube"})     = make_cube b
make_object b@(BlenderObject {type_="sphere"})   = make_sphere b
make_object b@(BlenderObject {type_="cylinder"}) = make_cylinder b
make_object b@(BlenderObject {type_="cone"})     = make_cone b
make_object b@(BlenderObject {type_= _ })        = make_sphere b


simple3operator name (x,y,z) = T.concat 
  [name, "([", T.pack $ show x, ", "
             , T.pack $ show y, ", "
             , T.pack $ show z,"])"]

translate = simple3operator "translate"
scale     = simple3operator "scale"

rotate3V a (rx,  ry, rz) = T.concat 
           ["rotate(a=", T.pack $ show a
              , " , v=[",T.pack $ show rx, ", ", T.pack $ show ry, ", "
                        ,T.pack $ show rz,"])"]



cube r (x,y,z) = T.concat 
            ["cube(r=",T.pack $ show r
            ," , center=true "
            ," , size=[", T.pack $ show x, ", ", T.pack $ show y, ", "
                        ,T.pack $ show z,"]);"]

sphere r = T.concat ["sphere(",T.pack $ show r, ");"]

cylinder r1 r2 h = T.concat 
  ["cylinder(r1=", T.pack $ show r1, " , r2=", T.pack $ show r2, " , h="
                 , T.pack $ show h, ");"]

make_cube :: BlenderObject -> Log (Maybe T.Text)
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
   | input_is_allright = return $ Just $ T.concat [
                             translate (x, y, z)
                            ," "
                            ,rotate3V (rw)  (rx,  ry, rz) 
                            ," "
                            ,cube rnd (dx, dy, dz)
                            ]
   |otherwise = do W.tell ["Invalid argument in make_cube: " , (B.pack $ show b)]
                   return Nothing
   where
   input_is_allright = (sx >= 0) && (sy >= 0) && (sz >= 0)


make_sphere :: BlenderObject -> Log (Maybe T.Text)
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
   | input_is_allright = return $ Just $ T.concat [
                                 translate (x, y, z)
                                , " " 
                                ,rotate3V (rw)  (rx,  ry, rz)
                                , " " 
                                ,scale (sx, sy, sz)
                                , " " 
                                ,sphere 1
                                ]
   |otherwise = do W.tell ["Invalid argument in make_sphere: " , (B.pack $ show b)]
                   return Nothing
   where
      input_is_allright = (sx >= 0) && (sy >= 0) && (sz >= 0)


make_cylinder :: BlenderObject -> Log (Maybe T.Text)
make_cylinder = make_cylinder_common 1 1 2 


make_cone :: BlenderObject -> Log (Maybe T.Text)
make_cone = make_cylinder_common 1 0 2 



make_cylinder_common :: Double -> Double -> Double -> BlenderObject 
                                                   -> Log (Maybe T.Text)
make_cylinder_common r1 r2 h b@(BlenderObject {x =x
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
   | input_is_allright = return $ Just $ T.concat [
                           translate (x, y, z) 
                          , " " 
                          ,rotate3V (rw)  (rx,  ry, rz) 
                          , " " 
                          ,scale (sx, sy, sz)
                          , " " 
                          ,cylinder r1 r2 h
                          ]
   |otherwise = do W.tell ["Invalid argument in make_cone: " , (B.pack $ show b)]
                   return Nothing
   where
      input_is_allright = (sx >= 0) && (sy >= 0) && (sz >= 0)

                                                   
                                                   
                                                   
