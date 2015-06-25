
import json
import io
import math
from os import system
from bpy.props import *

#def get_rotation(obj):
#    matrix_rotation = obj.orientation
#    euler_rotation = matrix_rotation.to_euler()
#    degrees_rotation = [math.degrees(a) for a in euler_rotation]
#    return degrees_rotation


def communicate_with_GenImplicit_via_json(json_file_name):

   j = []
   dummy = bpy.data.objects['Dummy']
   dummy.rotation_mode = 'XYZ'

   for obj in bpy.data.objects:
      if "toOpenSCAD" in obj:
         dummy.matrix_world = obj.matrix_world
         dummy.rotation_mode = 'AXIS_ANGLE'
         w_rotation, x_rotation, y_rotation, z_rotation  = dummy.rotation_axis_angle
         dummy.rotation_mode = 'XYZ'

         #x_rotation, y_rotation, z_rotation  = obj.rotation_euler
         #w_rotation, x_rotation, y_rotation, z_rotation  = obj.rotation_axis_angle
         j.append ({ 'name' : obj.name ,
                     'type_': obj["type"] ,
                     'group': list(map(lambda g: g.name, list(obj.users_group))),
                     'x'    : obj.location.x ,
                     'y'    : obj.location.y , 
                     'z'    : obj.location.z ,
                     'dim_x'    : obj.dimensions.x ,
                     'dim_y'    : obj.dimensions.y , 
                     'dim_z'    : obj.dimensions.z ,
                     'scale_x'  : abs(obj.scale.x) ,
                     'scale_y'  : abs(obj.scale.y) , 
                     'scale_z'  : abs(obj.scale.z) ,
                     'rot_x'  : x_rotation ,
                     'rot_y'  : y_rotation , 
                     'rot_z'  : z_rotation ,
                     'rot_w'  : w_rotation 
                  })
         if "rounding" in obj:
            j[-1].update({'rounding' : obj["rounding"]})
         else:
            j[-1].update({'rounding' : 0.0})
         #print(j)

   data = {
             'objects' : j 
            ,'groups'  : list(map(lambda g: g.name, bpy.data.groups)),
	       }


   #'/home/hokum/Documents/haskell/blender_haskell/gen_mesh/data.json'
   with io.open(json_file_name, 'w', encoding='utf-8') as f:
      json.dump(data, f, ensure_ascii=False)








def gen_implicit_render_handler_pre(self):
   #gen_implicit_render_handler_post(scn)
   scn = bpy.context.scene
   gen_implicit_import("." + str(scn.frame_current));
   #print("xxxxxxxxxxxdddddddddddddddd")





def gen_implicit_render_handler_post(self):
   scn = bpy.context.scene
   #postfix = "." + str(scn.frame_current)
   stl_file_name_base = scn.stl_file # + postfix
   name_of_import     = scn.name_of_import

   bpy.ops.object.select_all(action='DESELECT')
   bpy.ops.object.select_pattern(pattern=name_of_import)
   bpy.ops.object.delete() 

   #ob = scn.objects.get("Test2.Stl")
   #print(ob)
   #if ob is not None:
        #bpy.context.active_object.mode_set(mode='OBJECT')
        #bpy.ops.object.select_all(action='DESELECT')
        #ob.select = True
        #bpy.context.active_object.delete()
        #print("deleted!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-=-=-=-==========-=-=-")


   #print("xxxxxxxxxxxdddddddddddddddd")








def render_ready_callback(self, context):
   render_ready_callback___();







def render_ready_callback___():
   scn = bpy.context.scene
   render_ready = scn.render_ready

   if render_ready :

      if not gen_implicit_render_handler_pre.__name__ in [hand.__name__ for hand in bpy.app.handlers.render_pre]:
          bpy.app.handlers.render_pre.append(gen_implicit_render_handler_pre)

      if not gen_implicit_render_handler_post.__name__ in [hand.__name__ for hand in bpy.app.handlers.render_post]:
          bpy.app.handlers.render_post.append(gen_implicit_render_handler_post)

   else :

      bpy.app.handlers.render_pre.remove( gen_implicit_render_handler_pre )

      bpy.app.handlers.render_post.remove( gen_implicit_render_handler_post ())

















class GenImplicit(bpy.types.Panel):
    bl_label = "GenImplicit"
    bl_space_type = "VIEW_3D"
    bl_region_type = "TOOLS"
 

    
    def draw(self, context):

        layout = self.layout
        scn = context.scene
        layout.operator("gen_mesh.gen_mesh")
        layout.operator("gen_mesh.gen_mesh_import_current")
        #layout.prop(scn, 'mesh_quality_draft')
        layout.prop(scn, 'mesh_quality_full')
        layout.prop(scn, 'overall_rounding')
        layout.prop(scn, 'working_directory')
        layout.prop(scn, 'json_file')
        layout.prop(scn, 'stl_file')
        layout.prop(scn, 'name_of_import')
        layout.prop(scn, 'frame_start_')
        layout.prop(scn, 'frame_end_')
        layout.operator("gen_mesh.gen_mesh_anim")
        layout.prop(scn, 'json_only')
        layout.prop(scn, 'render_ready')
	     


def genImplicitProperties(scn):
   bpy.types.Scene.mesh_quality_draft = FloatProperty(
        name = "Mesh quality draft" 
        ,description = "Enter a float"
        ,default = 0.2
        ,min = 0.01
        #,max = 999999999999
        )

   bpy.types.Scene.mesh_quality_full = FloatProperty(
         name = "Mesh quality full" 
        ,description = "Enter a float"
        ,default = 0.01
        ,min = 0.01
        #,max = 999999999999
        )

   bpy.types.Scene.overall_rounding = FloatProperty(
         name = "Overall rounding" 
        ,description = "Enter a float"
        ,default = 0
        ,min = 0
        #,max = 999999999999
        )



   bpy.types.Scene.working_directory = StringProperty(
         name = "Working directory")
   scn['working_directory'] = "/media/sda2/sda2/temp/anim/"

   bpy.types.Scene.json_file = StringProperty(
         name = "Json file")
   scn['json_file'] = "data.json"

   bpy.types.Scene.stl_file = StringProperty(
         name = "stl file")
   scn['stl_file'] = "test2.stl"

   bpy.types.Scene.name_of_import = StringProperty(
         name = "Name of import")
   scn['name_of_import'] = "Test2.Stl"


   bpy.types.Scene.frame_start_ = IntProperty(
       name = "Frame start" 
      ,description = "Enter an integer"
      ,default = 0
      #,min = -9999999
      #,max =  9999999
      )
   #scn['frame_start_'] = '0'

   bpy.types.Scene.frame_end_ = IntProperty(
       name = "Frame end"
      ,description = "Enter an integer"
      ,default = 0
      #,min = -9999999
      #,max =  9999999
      )
   #scn['frame_end_'] = '0'

   bpy.types.Scene.json_only = BoolProperty(
       name = "Json only" 
      ,description = "Enter an Bool"
      ,default = False
      #,min = -9999999
      #,max =  9999999
      )

   bpy.types.Scene.render_ready = BoolProperty(
       name = "Render ready" 
      ,description = "Enter an Bool"
      ,default = False
      ,update = render_ready_callback
      #,min = -9999999
      #,max =  9999999
      )

   return
 
genImplicitProperties(bpy.context.scene)



class GEN_MESH_OT_GenImplicit(bpy.types.Operator):
    bl_idname = "gen_mesh.gen_mesh"
    bl_label = "Single update"

    #scn = bpy.context.scene
    #json_file_name = bpy.props.StringProperty()

    

    def execute(self, context):

      gen_implicit_general("")
      gen_implicit_import("")

      return{'FINISHED'}


class GEN_MESH_OT_GenImplicit_import_current(bpy.types.Operator):
    bl_idname = "gen_mesh.gen_mesh_import_current"
    bl_label = "Import current"

    #scn = bpy.context.scene
    #json_file_name = bpy.props.StringProperty()

    

    def execute(self, context):

      gen_implicit_import_current_frame()

      return{'FINISHED'}




class GEN_MESH_OT_GenImplicit_anim(bpy.types.Operator):
    bl_idname = "gen_mesh.gen_mesh_anim"
    bl_label = "Animation"

    def execute(self, context):
      gen_implicit_animation()
      
      return{'FINISHED'}





bpy.utils.register_module(__name__)


GenImplicit_exec   = '/home/hokum/Documents/haskell/blender_haskell/gen_mesh/genimplicit/.cabal-sandbox/bin/GenImplicit'




def gen_implicit_general(postfix):

   scn = bpy.context.scene

   json_file_name_base = scn.json_file + postfix
   json_file_dir       = scn.working_directory 

   stl_file_name_base = scn.stl_file + postfix
   stl_file_dir       = scn.working_directory

   json_only          = scn.json_only
    
   mesh_quality       = scn.mesh_quality_full

   overall_rounding   = scn.overall_rounding

   #print("Communicate to %s!" % self.json_file_name)
   #bpy.context.scene.frame_set(5)
   communicate_with_GenImplicit_via_json(json_file_dir + json_file_name_base);

   if not json_only:
      system( GenImplicit_exec + " --json-import-file " + json_file_dir + json_file_name_base 
                               + " --stl-export-file "  + stl_file_dir  + stl_file_name_base 
                               + " --mesh-quality "     + str(mesh_quality) 
                               + " --overall-union-rounding " + str(overall_rounding) );
      



def gen_implicit_import(postfix):

   scn = bpy.context.scene

   json_file_name_base = scn.json_file + postfix
   json_file_dir       = scn.working_directory 

   stl_file_name_base = scn.stl_file + postfix
   stl_file_dir       = scn.working_directory
    
   mesh_quality       = scn.mesh_quality_full

   bpy.ops.import_mesh.stl(filepath=stl_file_dir + stl_file_name_base
                        # , filter_glob="*.stl"
                         , files=[{"name": stl_file_name_base}] 
                         , directory=stl_file_dir);
   bpy.ops.object.shade_smooth();
   


def gen_implicit_import_current_frame():
   scn = bpy.context.scene

   #bpy.ops.object.select_all(action='DESELECT')
   #bpy.ops.object.select_pattern(pattern='Test2.Stl')
   #bpy.ops.object.delete() 

   #for ob in scn.objects:
   #   print (ob)
   #   ob.select = ob.type == 'MESH' and ob.name.startswith('Test2.Stl')
   #   bpy.ops.object.delete()


   gen_implicit_import("." + str(scn.frame_current));
   


def gen_implicit_animation():
   scn = bpy.context.scene

   for frame in range(scn.frame_start_, scn.frame_end_ + 1):
      bpy.context.scene.frame_set(frame)
      gen_implicit_general("." + str(frame))















