bl_info = {
    "name": "Mold Generator",
    "author": "Claude (Anthropic)",
    "version": (3, 1, 1),
    "blender": (4, 2, 0),
    "location": "View3D > Sidebar > Mold Generator",
    "description": "Generates a mold for the selected object with optional keys",
    "category": "Object",
}

import bpy
import bmesh
from bpy.props import (
    FloatProperty,
    IntProperty,
    EnumProperty,
    FloatVectorProperty,
    IntVectorProperty,
    PointerProperty,
    BoolProperty,
    StringProperty,
)
from bpy.types import (
    Operator,
    Panel,
    PropertyGroup,
)
from mathutils import Vector
import random
import math

# Utility function for debugging
def debug_print(message):
    """Utility function for debugging."""
    print(f"[Mold Generator DEBUG]: {message}")

# Utility function for unit conversion
def mm_to_bu(mm_value, context):
    """Convert millimeters to Blender Units based on scene settings."""
    unit_scale = context.scene.unit_settings.scale_length
    if abs(unit_scale - 0.001) < 0.0001:  # Scene is in mm
        return mm_value
    else:  # Scene is in meters or other units
        return mm_value * 0.001 / unit_scale

# Utility conversion from Blender units to millimeters
def bu_to_mm(bu_value, context):
    """Convert Blender Units to millimeters."""
    unit_scale = context.scene.unit_settings.scale_length
    if abs(unit_scale - 0.001) < 0.0001:
        return bu_value
    else:
        return bu_value * unit_scale * 1000.0

# Utility to get world-space bounds of an object
def get_world_bounds(obj):
    """Return (min, max) world coordinates of ``obj``'s bounding box."""
    bbox = [obj.matrix_world @ Vector(corner) for corner in obj.bound_box]
    min_co = Vector((
        min(v[0] for v in bbox),
        min(v[1] for v in bbox),
        min(v[2] for v in bbox)
    ))
    max_co = Vector((
        max(v[0] for v in bbox),
        max(v[1] for v in bbox),
        max(v[2] for v in bbox)
    ))
    return min_co, max_co

# Define a Property Group to hold the add-on properties
class MoldGeneratorProperties(PropertyGroup):
    padding: FloatProperty(
        name="Padding (mm)",
        description="Extra space around the object in millimeters",
        default=0.0,
        min=0.0,
        max=100.0,  # Max 100mm padding
        soft_max=20.0,  # Soft max at 20mm for UI
        step=0.1,
        precision=1,
        unit='LENGTH',
        subtype='DISTANCE',
        update=lambda self, context: self.update_mold_box(context)
    )
    cutting_axis: EnumProperty(
        name="Cutting Axis",
        description="Axis along which to split the mold",
        items=[
            ('X', 'X', 'X-Axis'),
            ('Y', 'Y', 'Y-Axis'),
            ('Z', 'Z', 'Z-Axis'),
        ],
        default='Z',
    )
    key_padding: FloatProperty(
        name="Key Edge Padding (mm)",
        description="Minimum space from mold edges for key placement",
        default=2.0,
        min=0.0,
        max=20.0,
        step=0.1,
        precision=1,
        unit='LENGTH',
        subtype='DISTANCE',
    )
    num_keys: IntProperty(
        name="Number of Keys",
        description="Number of alignment keys (max 32)",
        default=4,
        min=0,
        max=32,
    )
    key_radius: FloatProperty(
        name="Key Radius (mm)",
        description="Radius of alignment keys in millimeters",
        default=2.0,
        min=0.5,
        max=10.0,
        step=0.1,
        precision=1,
        unit='LENGTH',
        subtype='DISTANCE',
    )
    min_key_spacing: FloatProperty(
        name="Min Key Spacing (mm)",
        description="Minimum spacing between keys",
        default=5.0,
        min=1.0,
        max=20.0,
        step=0.1,
        precision=1,
        unit='LENGTH',
        subtype='DISTANCE',
    )
    hide_original: BoolProperty(
        name="Hide Original Object",
        description="Hide the original object after creating the mold",
        default=True,
    )
    # State management properties
    mold_state: EnumProperty(
        name="Mold State",
        items=[
            ('NONE', 'None', 'No mold created'),
            ('BOX_CREATED', 'Box Created', 'Mold box created but not applied'),
            ('MOLD_APPLIED', 'Mold Applied', 'Mold applied with boolean'),
            ('KEYS_ADDED', 'Keys Added', 'Keys added to mold'),
            ('COMPLETE', 'Complete', 'Mold complete with applied keys'),
        ],
        default='NONE',
    )
    original_object_name: StringProperty(
        name="Original Object Name",
        default="",
    )
    mold_collection_name: StringProperty(
        name="Mold Collection Name",
        default="",
    )

    def update_mold_box(self, context):
        """Update the mold box size live when padding is adjusted."""
        if self.mold_state != 'BOX_CREATED':
            return
            
        debug_print(f"Updating mold box with padding: {self.padding}mm")
        
        original_obj = bpy.data.objects.get(self.original_object_name)
        if not original_obj:
            debug_print(f"Original object '{self.original_object_name}' not found.")
            return

        mold_collection = bpy.data.collections.get(self.mold_collection_name)
        if not mold_collection:
            debug_print(f"Mold collection '{self.mold_collection_name}' not found.")
            return

        mold_whole = mold_collection.objects.get("mold.whole")
        if not mold_whole:
            debug_print("mold.whole object not found in mold collection.")
            return

        # Get padding in Blender units
        padding_bu = mm_to_bu(self.padding, context)
        
        # Get object's bounding box
        bbox_corners = [original_obj.matrix_world @ Vector(corner) for corner in original_obj.bound_box]
        
        min_corner = Vector((
            min(v[0] for v in bbox_corners),
            min(v[1] for v in bbox_corners),
            min(v[2] for v in bbox_corners)
        ))
        max_corner = Vector((
            max(v[0] for v in bbox_corners),
            max(v[1] for v in bbox_corners),
            max(v[2] for v in bbox_corners)
        ))
        
        # Add padding
        min_padded = min_corner - Vector((padding_bu, padding_bu, padding_bu))
        max_padded = max_corner + Vector((padding_bu, padding_bu, padding_bu))
        
        # Calculate new size and center
        center = (min_padded + max_padded) / 2.0
        size = max_padded - min_padded
        
        # Update mold box location
        mold_whole.location = center
        
        # Update mold box dimensions by scaling
        # First reset scale
        mold_whole.scale = (1, 1, 1)
        # Then set dimensions which will update scale
        mold_whole.dimensions = size
        
        debug_print(f"Updated mold box to center: {center}, size: {size}")

# Function to clean up orphaned data
def cleanup_orphaned_data():
    """Remove orphaned mesh data blocks."""
    for mesh in bpy.data.meshes:
        if mesh.users == 0:
            bpy.data.meshes.remove(mesh)

# Function to ensure proper context
def ensure_3d_context():
    """Ensure we have a 3D viewport context."""
    for area in bpy.context.screen.areas:
        if area.type == 'VIEW_3D':
            for space in area.spaces:
                if space.type == 'VIEW_3D':
                    return True
    return False

# Function to set X-ray mode for all 3D viewports
def set_xray_for_viewports(context):
    """Sets X-ray mode for all 3D viewports."""
    debug_print("Setting X-ray mode for all 3D viewports.")
    for area in context.screen.areas:
        if area.type == 'VIEW_3D':
            for space in area.spaces:
                if space.type == 'VIEW_3D':
                    if space.shading.type == 'WIREFRAME':
                        space.shading.show_xray_wireframe = True
                    else:
                        space.shading.show_xray = True

# Function to focus the viewport on a specific object
def focus_on_object(context, obj):
    """Focuses the viewport on the specified object."""
    if not ensure_3d_context():
        return
        
    debug_print(f"Focusing viewport on object: {obj.name}")
    # Select only the target object
    bpy.ops.object.select_all(action='DESELECT')
    obj.select_set(True)
    context.view_layer.objects.active = obj
    
    # Try to focus on selected
    try:
        override = context.copy()
        for area in context.screen.areas:
            if area.type == 'VIEW_3D':
                override['area'] = area
                override['region'] = area.regions[-1]
                bpy.ops.view3d.view_selected(override)
                break
    except:
        pass

# Function to apply and remove a Boolean modifier
def apply_boolean_modifier(obj, target, operation='DIFFERENCE'):
    """Applies a Boolean modifier to ``obj`` using ``target``."""
    debug_print(f"Applying Boolean modifier '{operation}' with target '{target.name}'.")

    # Ensure both objects have applied scale to avoid boolean issues
    obj.select_set(True)
    bpy.context.view_layer.objects.active = obj
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)

    bpy.ops.object.select_all(action='DESELECT')
    target.select_set(True)
    bpy.context.view_layer.objects.active = target
    bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)

    bpy.ops.object.select_all(action='DESELECT')
    obj.select_set(True)
    bpy.context.view_layer.objects.active = obj

    bool_mod = obj.modifiers.new(name='Boolean', type='BOOLEAN')
    bool_mod.operation = operation
    bool_mod.object = target
    bool_mod.solver = 'EXACT'
    
    try:
        bpy.ops.object.modifier_apply(modifier=bool_mod.name)
        debug_print(f"Applied Boolean modifier '{operation}' to '{obj.name}'.")
        return True
    except Exception as e:
        debug_print(f"Failed to apply Boolean modifier '{operation}' to '{obj.name}': {e}")
        obj.modifiers.remove(bool_mod)
        return False

# Heuristic to estimate a suitable key radius in millimeters
def estimate_key_radius(mold_obj, original_obj, cutting_axis, key_padding, context):
    axis_idx = {'X': 0, 'Y': 1, 'Z': 2}[cutting_axis]
    other_axes = [i for i in [0, 1, 2] if i != axis_idx]

    mold_min, mold_max = get_world_bounds(mold_obj)
    orig_min, orig_max = get_world_bounds(original_obj)

    pad = mm_to_bu(key_padding, context)

    margin1 = min(orig_min[other_axes[0]] - mold_min[other_axes[0]],
                  mold_max[other_axes[0]] - orig_max[other_axes[0]]) - pad
    margin2 = min(orig_min[other_axes[1]] - mold_min[other_axes[1]],
                  mold_max[other_axes[1]] - orig_max[other_axes[1]]) - pad

    if margin1 <= 0 or margin2 <= 0:
        return 1.0  # Fallback to small radius (mm)

    radius_bu = max(min(margin1, margin2) * 0.4, mm_to_bu(0.5, context))
    radius_mm = bu_to_mm(radius_bu, context)
    return max(0.5, min(radius_mm, 10.0))

# Simplified function to find valid key positions
def find_key_positions(mold_obj, original_obj, cutting_axis, num_keys, key_radius,
                       key_padding, min_spacing, context, cut_pos=None):
    """Find valid positions for alignment keys using a simple grid approach."""
    debug_print("Finding key positions using simplified grid approach")
    
    # Convert measurements from mm to Blender units
    key_radius_bu = mm_to_bu(key_radius, context)
    key_padding_bu = mm_to_bu(key_padding, context)
    min_spacing_bu = mm_to_bu(min_spacing, context)
    
    axis_index = {'X': 0, 'Y': 1, 'Z': 2}[cutting_axis]
    other_axes = [i for i in [0, 1, 2] if i != axis_index]
    
    # Get mold bounds
    mold_bbox = [mold_obj.matrix_world @ Vector(corner) for corner in mold_obj.bound_box]
    mold_min = Vector((
        min(v[0] for v in mold_bbox),
        min(v[1] for v in mold_bbox),
        min(v[2] for v in mold_bbox)
    ))
    mold_max = Vector((
        max(v[0] for v in mold_bbox),
        max(v[1] for v in mold_bbox),
        max(v[2] for v in mold_bbox)
    ))
    
    # Get original object bounds
    orig_bbox = [original_obj.matrix_world @ Vector(corner) for corner in original_obj.bound_box]
    orig_min = Vector((
        min(v[0] for v in orig_bbox),
        min(v[1] for v in orig_bbox),
        min(v[2] for v in orig_bbox)
    ))
    orig_max = Vector((
        max(v[0] for v in orig_bbox),
        max(v[1] for v in orig_bbox),
        max(v[2] for v in orig_bbox)
    ))
    
    # Cutting plane position provided by caller or derived from mold bounds
    if cut_pos is None:
        cut_pos = (mold_min[axis_index] + mold_max[axis_index]) / 2.0
    
    # Define safe zones - areas outside the original object's bounding box projection
    axis1, axis2 = other_axes
    
    # Calculate placement bounds
    placement_min1 = mold_min[axis1] + key_padding_bu + key_radius_bu
    placement_max1 = mold_max[axis1] - key_padding_bu - key_radius_bu
    placement_min2 = mold_min[axis2] + key_padding_bu + key_radius_bu
    placement_max2 = mold_max[axis2] - key_padding_bu - key_radius_bu
    
    # Object bounds with safety margin
    obj_min1 = orig_min[axis1] - key_radius_bu * 0.5
    obj_max1 = orig_max[axis1] + key_radius_bu * 0.5
    obj_min2 = orig_min[axis2] - key_radius_bu * 0.5
    obj_max2 = orig_max[axis2] + key_radius_bu * 0.5
    
    positions = []
    attempts = 0
    max_attempts = 1000  # Prevent infinite loops
    
    # Try to place keys in corners and edges first (usually safe spots)
    # Corner positions
    corner_positions = [
        (placement_min1, placement_min2),
        (placement_min1, placement_max2),
        (placement_max1, placement_min2),
        (placement_max1, placement_max2),
    ]
    
    # Edge center positions
    edge_positions = [
        ((placement_min1 + placement_max1) / 2, placement_min2),
        ((placement_min1 + placement_max1) / 2, placement_max2),
        (placement_min1, (placement_min2 + placement_max2) / 2),
        (placement_max1, (placement_min2 + placement_max2) / 2),
    ]
    
    # Try corner and edge positions first
    for pos_2d in corner_positions + edge_positions:
        if len(positions) >= num_keys:
            break
            
        pos1, pos2 = pos_2d
        
        # Check if position is outside object bounds
        if (pos1 < obj_min1 or pos1 > obj_max1) or (pos2 < obj_min2 or pos2 > obj_max2):
            # Check spacing from existing keys
            pos_3d = [0, 0, 0]
            pos_3d[axis_index] = cut_pos
            pos_3d[axis1] = pos1
            pos_3d[axis2] = pos2
            pos_vec = Vector(pos_3d)
            
            valid = True
            for existing_pos in positions:
                if (pos_vec - existing_pos).length < min_spacing_bu:
                    valid = False
                    break
            
            if valid:
                positions.append(pos_vec)
    
    # If we need more keys, try random positions
    while len(positions) < num_keys and attempts < max_attempts:
        attempts += 1
        
        # Generate random position
        pos1 = random.uniform(placement_min1, placement_max1)
        pos2 = random.uniform(placement_min2, placement_max2)
        
        # Check if position is outside object bounds
        if (pos1 < obj_min1 or pos1 > obj_max1) or (pos2 < obj_min2 or pos2 > obj_max2):
            # Check spacing from existing keys
            pos_3d = [0, 0, 0]
            pos_3d[axis_index] = cut_pos
            pos_3d[axis1] = pos1
            pos_3d[axis2] = pos2
            pos_vec = Vector(pos_3d)
            
            valid = True
            for existing_pos in positions:
                if (pos_vec - existing_pos).length < min_spacing_bu:
                    valid = False
                    break
            
            if valid:
                positions.append(pos_vec)
    
    debug_print(f"Found {len(positions)} valid key positions out of {num_keys} requested")
    return positions

# Operator to add the mold box
class OBJECT_OT_AddMoldBoxOperator(Operator):
    bl_idname = "object.add_mold_box"
    bl_label = "Add Mold Box"
    bl_description = "Create the initial mold box with padding set to 0."
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        props = context.scene.mold_generator_props
        
        # Reset state
        props.mold_state = 'NONE'
        props.padding = 0.0  # Start with 0 padding
        
        debug_print("Starting mold box creation process.")
        
        # Check scene units
        unit_scale = context.scene.unit_settings.scale_length
        if abs(unit_scale - 0.001) < 0.0001:
            debug_print("Scene units detected: millimeters")
        else:
            debug_print(f"Scene units detected: meters or custom (scale={unit_scale})")

        obj = context.active_object
        if not obj or obj.type != 'MESH':
            self.report({'WARNING'}, "Please select a mesh object.")
            return {'CANCELLED'}

        debug_print(f"Selected object: {obj.name}")
        
        # Clean up any existing mold for this object
        if props.mold_collection_name:
            old_collection = bpy.data.collections.get(props.mold_collection_name)
            if old_collection:
                # Remove all objects in the collection
                for mold_obj in old_collection.objects:
                    bpy.data.objects.remove(mold_obj, do_unlink=True)
                bpy.data.collections.remove(old_collection)
        
        # Apply transformations to the selected object
        obj.select_set(True)
        bpy.context.view_layer.objects.active = obj
        bpy.ops.object.transform_apply(location=True, rotation=True, scale=True)
        debug_print("Applied transformations to the selected object.")

        # Get object's bounding box
        bbox_corners = [obj.matrix_world @ Vector(corner) for corner in obj.bound_box]
        
        min_corner = Vector((
            min(v[0] for v in bbox_corners),
            min(v[1] for v in bbox_corners),
            min(v[2] for v in bbox_corners)
        ))
        max_corner = Vector((
            max(v[0] for v in bbox_corners),
            max(v[1] for v in bbox_corners),
            max(v[2] for v in bbox_corners)
        ))

        # Initial size with 0 padding
        center = (min_corner + max_corner) / 2.0
        size = max_corner - min_corner

        debug_print(f"Object bounds - Min: {min_corner}, Max: {max_corner}")
        debug_print(f"Initial mold center: {center}, size: {size}")

        # Create mold box
        bpy.ops.mesh.primitive_cube_add(size=2, location=center)
        mold_box = context.active_object
        mold_box.name = "mold.whole"
        
        # Set dimensions (this will update the scale)
        mold_box.dimensions = size

        # Create collection for mold parts
        mold_collection_name = f"Mold_{obj.name}"
        mold_collection = bpy.data.collections.new(mold_collection_name)
        context.scene.collection.children.link(mold_collection)
        
        # Move mold box to collection
        mold_collection.objects.link(mold_box)
        context.collection.objects.unlink(mold_box)

        # Store references
        props.original_object_name = obj.name
        props.mold_collection_name = mold_collection_name
        props.mold_state = 'BOX_CREATED'

        # Set viewport
        set_xray_for_viewports(context)
        
        # Change view based on cutting axis
        if ensure_3d_context():
            cutting_axis = props.cutting_axis
            try:
                if cutting_axis == 'X':
                    bpy.ops.view3d.view_axis(type='RIGHT')
                elif cutting_axis == 'Y':
                    bpy.ops.view3d.view_axis(type='FRONT')
                elif cutting_axis == 'Z':
                    bpy.ops.view3d.view_axis(type='TOP')
            except:
                pass

        focus_on_object(context, obj)
        
        # Re-select original object
        bpy.ops.object.select_all(action='DESELECT')
        obj.select_set(True)
        context.view_layer.objects.active = obj

        self.report({'INFO'}, "Mold box added. Adjust padding as needed before applying.")
        return {'FINISHED'}

# Operator to apply the mold box
class OBJECT_OT_ApplyMoldBoxOperator(Operator):
    bl_idname = "object.apply_mold_box"
    bl_label = "Apply Mold Box"
    bl_description = "Finalize the mold by performing boolean operations."
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        props = context.scene.mold_generator_props
        
        if props.mold_state != 'BOX_CREATED':
            self.report({'WARNING'}, "Please create a mold box first.")
            return {'CANCELLED'}

        debug_print("Starting mold finalization process.")

        original_obj = bpy.data.objects.get(props.original_object_name)
        if not original_obj:
            self.report({'WARNING'}, "Original object not found.")
            return {'CANCELLED'}

        mold_collection = bpy.data.collections.get(props.mold_collection_name)
        if not mold_collection:
            self.report({'WARNING'}, "Mold collection not found.")
            return {'CANCELLED'}

        mold_whole = mold_collection.objects.get("mold.whole")
        if not mold_whole:
            self.report({'WARNING'}, "Mold box not found.")
            return {'CANCELLED'}

        # Apply scale to mold before boolean
        mold_whole.select_set(True)
        context.view_layer.objects.active = mold_whole
        bpy.ops.object.transform_apply(location=False, rotation=False, scale=True)

        # Apply boolean difference
        if not apply_boolean_modifier(mold_whole, original_obj, operation='DIFFERENCE'):
            self.report({'ERROR'}, "Boolean operation failed.")
            return {'CANCELLED'}

        # Create mold halves
        cutting_axis = props.cutting_axis
        axis_index = {'X': 0, 'Y': 1, 'Z': 2}[cutting_axis]
        
        # Get mold bounds
        bbox = [mold_whole.matrix_world @ Vector(corner) for corner in mold_whole.bound_box]
        min_co = Vector((min(v[0] for v in bbox), min(v[1] for v in bbox), min(v[2] for v in bbox)))
        max_co = Vector((max(v[0] for v in bbox), max(v[1] for v in bbox), max(v[2] for v in bbox)))
        center = (min_co + max_co) / 2.0
        size = max_co - min_co

        # Create mold A
        mold_A = mold_whole.copy()
        mold_A.data = mold_whole.data.copy()
        mold_A.name = "mold.A"
        mold_collection.objects.link(mold_A)

        # Create cutting cube for A
        cut_size = size * 2
        cut_location = center.copy()
        cut_location[axis_index] += size[axis_index]
        
        bpy.ops.mesh.primitive_cube_add(size=2, location=cut_location)
        cut_cube_A = context.active_object
        cut_cube_A.dimensions = cut_size
        
        if not apply_boolean_modifier(mold_A, cut_cube_A, operation='INTERSECT'):
            self.report({'ERROR'}, "Boolean operation failed for mold A.")
            bpy.data.objects.remove(cut_cube_A, do_unlink=True)
            bpy.data.objects.remove(mold_A, do_unlink=True)
            return {'CANCELLED'}
        
        bpy.data.objects.remove(cut_cube_A, do_unlink=True)

        # Create mold B
        mold_B = mold_whole.copy()
        mold_B.data = mold_whole.data.copy()
        mold_B.name = "mold.B"
        mold_collection.objects.link(mold_B)

        # Create cutting cube for B
        cut_location[axis_index] -= size[axis_index] * 2
        
        bpy.ops.mesh.primitive_cube_add(size=2, location=cut_location)
        cut_cube_B = context.active_object
        cut_cube_B.dimensions = cut_size
        
        if not apply_boolean_modifier(mold_B, cut_cube_B, operation='INTERSECT'):
            self.report({'ERROR'}, "Boolean operation failed for mold B.")
            bpy.data.objects.remove(cut_cube_B, do_unlink=True)
            bpy.data.objects.remove(mold_B, do_unlink=True)
            return {'CANCELLED'}
        
        bpy.data.objects.remove(cut_cube_B, do_unlink=True)

        # Remove original mold box
        bpy.data.objects.remove(mold_whole, do_unlink=True)

        # Hide original if requested
        if props.hide_original:
            original_obj.hide_set(True)

        # Update state
        props.mold_state = 'MOLD_APPLIED'

        # Clean up orphaned data
        cleanup_orphaned_data()

        # Select mold A
        bpy.ops.object.select_all(action='DESELECT')
        mold_A.select_set(True)
        context.view_layer.objects.active = mold_A

        self.report({'INFO'}, f"Mold split along {cutting_axis}-axis successfully.")
        return {'FINISHED'}

# Operator to add keys
class OBJECT_OT_AddKeysOperator(Operator):
    bl_idname = "object.add_keys"
    bl_label = "Add Keys"
    bl_description = "Add alignment keys to the mold"
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        props = context.scene.mold_generator_props
        
        if props.mold_state not in ['MOLD_APPLIED', 'KEYS_ADDED']:
            self.report({'WARNING'}, "Please apply the mold first.")
            return {'CANCELLED'}

        debug_print("Starting key addition process.")

        original_obj = bpy.data.objects.get(props.original_object_name)
        if not original_obj:
            self.report({'WARNING'}, "Original object not found.")
            return {'CANCELLED'}

        mold_collection = bpy.data.collections.get(props.mold_collection_name)
        if not mold_collection:
            self.report({'WARNING'}, "Mold collection not found.")
            return {'CANCELLED'}

        mold_A = mold_collection.objects.get("mold.A")
        mold_B = mold_collection.objects.get("mold.B")
        if not mold_A or not mold_B:
            self.report({'WARNING'}, "Mold halves not found.")
            return {'CANCELLED'}

        # Remove existing keys
        for col_name in ["keys.A", "keys.B"]:
            keys_col = mold_collection.children.get(col_name)
            if keys_col:
                for obj in keys_col.objects:
                    bpy.data.objects.remove(obj, do_unlink=True)
                bpy.data.collections.remove(keys_col)

        # Determine cutting plane position from both mold halves
        axis_idx = {'X': 0, 'Y': 1, 'Z': 2}[props.cutting_axis]
        min_a, max_a = get_world_bounds(mold_A)
        min_b, max_b = get_world_bounds(mold_B)
        center_a = (min_a + max_a) / 2.0
        center_b = (min_b + max_b) / 2.0
        cut_plane = (center_a[axis_idx] + center_b[axis_idx]) / 2.0

        # Estimate a suitable key radius based on available space
        auto_radius = estimate_key_radius(
            mold_A, original_obj, props.cutting_axis,
            props.key_padding, context
        )
        props.key_radius = auto_radius

        # Find key positions with the computed radius
        key_positions = find_key_positions(
            mold_A, original_obj, props.cutting_axis,
            props.num_keys, auto_radius,
            props.key_padding, props.min_key_spacing,
            context, cut_plane
        )

        if not key_positions:
            self.report({'WARNING'}, "No valid key positions found. Try adjusting parameters.")
            return {'CANCELLED'}

        # Convert key radius to Blender units
        key_radius_bu = mm_to_bu(props.key_radius, context)

        # Create keys collections
        keys_A_col = bpy.data.collections.new("keys.A")
        mold_collection.children.link(keys_A_col)
        
        keys_B_col = bpy.data.collections.new("keys.B")
        mold_collection.children.link(keys_B_col)

        # Create keys
        axis_index = {'X': 0, 'Y': 1, 'Z': 2}[props.cutting_axis]
        
        for i, pos in enumerate(key_positions):
            # Create key A (positive)
            bpy.ops.mesh.primitive_uv_sphere_add(
                radius=key_radius_bu,
                location=pos,
                segments=16,
                ring_count=8
            )
            key_A = context.active_object
            key_A.name = f"key.A.{i:03d}"
            
            # Move to collection
            for coll in key_A.users_collection:
                coll.objects.unlink(key_A)
            keys_A_col.objects.link(key_A)
            
            # Add constraint to lock movement along cutting axis
            constraint = key_A.constraints.new(type='LIMIT_LOCATION')
            if axis_index == 0:
                constraint.use_min_x = constraint.use_max_x = True
                constraint.min_x = constraint.max_x = pos.x
            elif axis_index == 1:
                constraint.use_min_y = constraint.use_max_y = True
                constraint.min_y = constraint.max_y = pos.y
            else:
                constraint.use_min_z = constraint.use_max_z = True
                constraint.min_z = constraint.max_z = pos.z
            constraint.owner_space = 'WORLD'
            
            # Create key B (negative)
            key_B = key_A.copy()
            key_B.data = key_A.data.copy()
            key_B.name = f"key.B.{i:03d}"
            
            for coll in key_B.users_collection:
                coll.objects.unlink(key_B)
            keys_B_col.objects.link(key_B)

            # Slightly enlarge the indentation key for better fit
            key_B.scale *= 1.03

            # Keep location/rotation synced but preserve scale
            c_loc = key_B.constraints.new(type='COPY_LOCATION')
            c_loc.target = key_A
            c_rot = key_B.constraints.new(type='COPY_ROTATION')
            c_rot.target = key_A

        props.mold_state = 'KEYS_ADDED'
        
        self.report({'INFO'}, f"Added {len(key_positions)} alignment keys.")
        return {'FINISHED'}

# Operator to apply keys
class OBJECT_OT_ApplyKeysOperator(Operator):
    bl_idname = "object.apply_keys"
    bl_label = "Apply Keys"
    bl_description = "Perform boolean operations to integrate keys into the molds"
    bl_options = {'REGISTER', 'UNDO'}

    def execute(self, context):
        props = context.scene.mold_generator_props
        
        if props.mold_state != 'KEYS_ADDED':
            self.report({'WARNING'}, "Please add keys first.")
            return {'CANCELLED'}

        debug_print("Starting to apply keys to molds.")

        mold_collection = bpy.data.collections.get(props.mold_collection_name)
        if not mold_collection:
            self.report({'WARNING'}, "Mold collection not found.")
            return {'CANCELLED'}

        mold_A = mold_collection.objects.get("mold.A")
        mold_B = mold_collection.objects.get("mold.B")
        keys_A_col = mold_collection.children.get("keys.A")
        keys_B_col = mold_collection.children.get("keys.B")

        if not all([mold_A, mold_B, keys_A_col, keys_B_col]):
            self.report({'WARNING'}, "Required objects not found.")
            return {'CANCELLED'}

        # Apply keys to mold A (union)
        success = True
        for key in keys_A_col.objects:
            if not apply_boolean_modifier(mold_A, key, operation='UNION'):
                success = False
                break

        if success:
            # Apply keys to mold B (difference)
            for key in keys_B_col.objects:
                if not apply_boolean_modifier(mold_B, key, operation='DIFFERENCE'):
                    success = False
                    break

        if not success:
            self.report({'ERROR'}, "Boolean operations failed.")
            return {'CANCELLED'}

        # Hide key collections
        def set_collection_visibility(layer_coll, coll_name, exclude):
            for child in layer_coll.children:
                if child.collection.name == coll_name:
                    child.exclude = exclude
                    return True
                if set_collection_visibility(child, coll_name, exclude):
                    return True
            return False

        view_layer = context.view_layer.layer_collection
        set_collection_visibility(view_layer, "keys.A", True)
        set_collection_visibility(view_layer, "keys.B", True)

        props.mold_state = 'COMPLETE'
        
        # Clean up orphaned data
        cleanup_orphaned_data()

        # Select mold A
        bpy.ops.object.select_all(action='DESELECT')
        mold_A.select_set(True)
        context.view_layer.objects.active = mold_A

        self.report({'INFO'}, "Keys applied successfully. Mold complete!")
        return {'FINISHED'}

# Panel in the Sidebar
class VIEW3D_PT_MoldGeneratorPanel(Panel):
    bl_label = "Mold Generator"
    bl_idname = "VIEW3D_PT_mold_generator_panel"
    bl_space_type = 'VIEW_3D'
    bl_region_type = 'UI'
    bl_category = "Mold Generator"

    def draw(self, context):
        layout = self.layout
        props = context.scene.mold_generator_props

        # State indicator
        state_text = {
            'NONE': "No mold created",
            'BOX_CREATED': "Mold box created - adjust padding",
            'MOLD_APPLIED': "Mold ready for keys",
            'KEYS_ADDED': "Keys added - ready to apply",
            'COMPLETE': "Mold complete"
        }
        
        box = layout.box()
        box.label(text=f"Status: {state_text.get(props.mold_state, 'Unknown')}")
        
        layout.separator()

        # Parameters
        col = layout.column(align=True)
        col.label(text="Mold Parameters:")
        col.prop(props, "padding")
        col.prop(props, "cutting_axis")
        col.prop(props, "hide_original")
        
        layout.separator()
        
        col = layout.column(align=True)
        col.label(text="Key Parameters:")
        col.prop(props, "num_keys")
        col.prop(props, "key_radius")
        col.prop(props, "key_padding")
        col.prop(props, "min_key_spacing")

        layout.separator()

        # Action buttons based on state
        col = layout.column(align=True)
        
        if props.mold_state == 'NONE':
            col.operator("object.add_mold_box", icon='MESH_CUBE')
        elif props.mold_state == 'BOX_CREATED':
            col.operator("object.apply_mold_box", icon='MOD_BOOLEAN')
        elif props.mold_state == 'MOLD_APPLIED':
            col.operator("object.add_keys", icon='EMPTY_SINGLE_ARROW')
        elif props.mold_state == 'KEYS_ADDED':
            col.operator("object.apply_keys", icon='CHECKMARK')
            row = col.row(align=True)
            row.operator("object.add_keys", text="Regenerate Keys", icon='FILE_REFRESH')
        elif props.mold_state == 'COMPLETE':
            col.label(text="Mold complete!", icon='CHECKMARK')
            col.operator("object.add_mold_box", text="Create New Mold", icon='ADD')

        layout.separator()

        # Help
        box = layout.box()
        box.label(text="Quick Guide:", icon='INFO')
        box.label(text="1. Select mesh object")
        box.label(text="2. Add mold box (padding=0)")
        box.label(text="3. Adjust padding (mm)")
        box.label(text="4. Apply mold")
        box.label(text="5. Add & apply keys")

# Register and unregister
classes = (
    MoldGeneratorProperties,
    OBJECT_OT_AddMoldBoxOperator,
    OBJECT_OT_ApplyMoldBoxOperator,
    OBJECT_OT_AddKeysOperator,
    OBJECT_OT_ApplyKeysOperator,
    VIEW3D_PT_MoldGeneratorPanel,
)

def register():
    for cls in classes:
        bpy.utils.register_class(cls)
    bpy.types.Scene.mold_generator_props = PointerProperty(type=MoldGeneratorProperties)
    debug_print("Mold Generator addon registered.")

def unregister():
    for cls in reversed(classes):
        bpy.utils.unregister_class(cls)
    del bpy.types.Scene.mold_generator_props
    debug_print("Mold Generator addon unregistered.")

if __name__ == "__main__":
    register()
