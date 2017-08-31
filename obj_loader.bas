'#####################################################
'#                OpenGL OBJ Loader                  #
'#            Author :- Ashish Kushwaha              #
'#   Special Thanks:- Waltersmind, Tylar Darko,      #
'#   and Steve.                                      #
'#  Try to load all models located in Models/*.obj   #
'#####################################################
'--------------------------------------------------------------------------------------------------
'****************************************************************************************
'**  To load a model, replace model_file$ with your file name                          **
'**  or simple drag a file on the executable. This OBJ loader also supports            **
'**  command-line arguements. Syntax is -                                              **
'**  obj_loader [model_file] [optional_texture_file]                                   **
'****************************************************************************************
'-------------------------------------------------------------------------------------------------
'****************************************************************************************
'**  Some the features of this OBJ loader is -                                         **
'**  1. Can Handle Multiple Objects with/without their material & textures.            **
'**  2. Have support for command-line.                                                 **
'**  Use -> obj_loader [model_file] [optional_texture_file]                            **
'**  3. Users can view wire-frame model by pressing space from keyboard.               **
'**  4. Support alpha blending.                                                        **
'**  5. Support material (.mtl) files.
'****************************************************************************************
'****************************************************************************************
'Found any bug, or, the loaded model doesn't seem to render accurately in this?
'Then please report your bug at
'or message me at twitter with @KingOfCoders
'or email me at ashishkushwahacb@gmail.com

'Have Fun!

'QB64 Rocks!! :)

_GLRENDER _BEHIND

DIM SHARED model_texture_file$, model_file$, material_file$

material_file$ = ""
model_file$ = "models/pyramid.obj"
model_texture_file$ = ""
IF _COMMANDCOUNT = 1 OR _COMMANDCOUNT = 2 THEN model_file$ = COMMAND$(1)

'this variable can be set to "", if you don't have textures.
model_texture_file$ = ""
IF _COMMANDCOUNT = 1 THEN model_texture_file$ = ""
IF _COMMANDCOUNT = 2 THEN model_texture_file$ = COMMAND$(2)

_TITLE "OpenGL OBJ Loader - " + model_file$

'finding the model directory
for i = len(model_file$) to 1 step -1
    ca$ = mid$(model_file$,i,1)
	if ca$ = "/" or ca$ = "\" then path_to_model$ = left$(model_file$,i) : exit for
next

SCREEN _NEWIMAGE(1000, 700, 32)

DIM SHARED glAllow AS _BYTE
DECLARE LIBRARY
    SUB gluLookAt (BYVAL eyeX#, BYVAL eyeY#, BYVAL eyeZ#, BYVAL centerX#, BYVAL centerY#, BYVAL centerZ#, BYVAL upX#, BYVAL upY#, BYVAL upZ#)
END DECLARE

'Used to manage textures
TYPE DONT_USE_GLH_Handle_TYPE
    in_use AS _BYTE
    handle AS LONG
END TYPE

TYPE vec3
    x AS SINGLE
    y AS SINGLE
    z AS SINGLE
END TYPE

TYPE triangular_face_vertex
    v AS vec3 'vertex
    vt AS vec3 'texture coordinate at this vertex
    vn AS vec3 'vertex normal (to interact with lights).
END TYPE

TYPE model_material
    ambient AS vec3
    diffuse AS vec3
    specular AS vec3
	emission as vec3
    tranparency AS DOUBLE
    shineness AS DOUBLE
    texture AS LONG
    gl_tex AS LONG
    mtl_name AS STRING * 128
    id AS _UNSIGNED LONG
END TYPE

TYPE triangular_face
    v1 AS triangular_face_vertex
    v2 AS triangular_face_vertex
    v3 AS triangular_face_vertex
    material AS model_material
    obj_name AS STRING * 128
    obj_id AS _UNSIGNED LONG
END TYPE


f = FREEFILE

o_count = 0
v_count = 0
vt_count = 0
vn_count = 0
face_count = 0

PRINT "Getting Information about model. Please be patient."

OPEN model_file$ FOR INPUT AS #f
WHILE NOT EOF(f)
    LINE INPUT #f, info$
    IF LEFT$(info$, 2) = "v " THEN v_count = v_count + 1
    IF LEFT$(info$, 2) = "vt" THEN vt_count = vt_count + 1
    IF LEFT$(info$, 2) = "vn" THEN vn_count = vn_count + 1
    IF LEFT$(info$, 2) = "f " THEN face_count = face_count + 1
    IF LEFT$(info$, 2) = "o " THEN o_count = o_count + 1
    IF LEFT$(info$, 6) = "mtllib" THEN 
	    material_file$ = Word$(info$, 2)
		if instr(material_file$, ":") = 0 then material_file$ = path_to_model$+material_file$
	end if
WEND
CLOSE #f

PRINT "Model has - "
PRINT "Total Objects : "; o_count
PRINT "Vertices : "; v_count
PRINT "Texture Coordinates (UVs) : "; vt_count
PRINT "Normals : "; vn_count
IF LEN(material_file$) = 0 THEN msg$ = "[none]" ELSE msg$ = material_file$
PRINT "Material Library : "; msg$
PRINT "Press any key to continue"
a$ = INPUT$(1)
PRINT
PRINT "Loading Model."


DIM SHARED totalObjects
totalObjects = o_count
DIM vertices(v_count) AS vec3
DIM texture_coordinates(vt_count) AS vec3
DIM normals(vn_count) AS vec3
' dim objects(o_count) as model_objects
DIM SHARED model_faces(face_count) AS triangular_face

'################ Materials #########################
IF material_file$ <> "" AND _FILEEXISTS(material_file$) THEN
    PRINT "Reading Materials"
    mtl_count = 0
    OPEN material_file$ FOR INPUT AS #f
    WHILE NOT EOF(f)
        LINE INPUT #f, info$
        IF LEFT$(info$, 6) = "newmtl" THEN mtl_count = mtl_count + 1
    WEND
    CLOSE #f
    PRINT "Total Materials : "; mtl_count
    
    DIM SHARED material(mtl_count) AS model_material
    mtl = 0
    OPEN material_file$ FOR INPUT AS #f
    WHILE NOT EOF(f)
        LINE INPUT #f, info$
        IF LEFT$(info$, 6) = "newmtl" THEN mtl = mtl + 1: material(mtl).mtl_name = Word$(info$, 2): material(mtl).id = mtl
        'ambient
        IF LEFT$(info$, 2) = "Ka" THEN
            material(mtl).ambient.x = VAL(Word$(info$, 2))
            material(mtl).ambient.y = VAL(Word$(info$, 3))
            material(mtl).ambient.z = VAL(Word$(info$, 4))
        END IF
        'diffuse
        IF LEFT$(info$, 2) = "Kd" THEN
            material(mtl).diffuse.x = VAL(Word$(info$, 2))
            material(mtl).diffuse.y = VAL(Word$(info$, 3))
            material(mtl).diffuse.z = VAL(Word$(info$, 4))
        END IF
        'specular
        IF LEFT$(info$, 2) = "Ks" THEN
            material(mtl).specular.x = VAL(Word$(info$, 2))
            material(mtl).specular.y = VAL(Word$(info$, 3))
            material(mtl).specular.z = VAL(Word$(info$, 4))
        END IF
		'emission
        IF LEFT$(info$, 2) = "Ke" THEN
            material(mtl).emission.x = VAL(Word$(info$, 2))
            material(mtl).emission.y = VAL(Word$(info$, 3))
            material(mtl).emission.z = VAL(Word$(info$, 4))
        END IF
        'shineness
        IF LEFT$(info$, 2) = "Ns" THEN
            material(mtl).shineness = VAL(Word$(info$, 2))
        END IF
        'tranparency
        IF LEFT$(info$, 2) = "d " THEN
            material(mtl).tranparency = VAL(Word$(info$, 2))
        END IF
        'texture mapping
        IF LEFT$(info$, 6) = "map_Kd" THEN
            temp_image_file$ = Word$(info$, 2)
			if instr(temp_image_file$, ":") = 0 then temp_image_file$ = path_to_model$+temp_image_file$
            PRINT "Attempting to Load Image : - "; temp_image_file$
            IF _FILEEXISTS(temp_image_file$) THEN
                PRINT "Image exists : "; temp_image_file$
                temp_image_handle = _LOADIMAGE(temp_image_file$, 32)
                IF temp_image_handle < -1 THEN
                    material(mtl).texture = temp_image_handle
                    PRINT "Image loaded successfully : "; temp_image_file$
                else
				    print "Failed to load image : ";temp_image_file$
                END IF
            ELSE
                PRINT temp_image_file$; " : file doesn't exists"
            END IF
        END IF
        PRINT ".";
    WEND
    CLOSE #f
END IF

v = 1
vt = 1
vn = 1
fc = 1
o = 0


' f = freefile
OPEN model_file$ FOR INPUT AS #f
WHILE NOT EOF(f)
    LINE INPUT #f, model_data$
    SELECT CASE LEFT$(model_data$, 2)
        CASE "o "
            o = o + 1
            current_object_name$ = Word$(model_data$, 2)
            PRINT "[Object:- "; current_object_name$; "]"
        CASE "us"
            IF Word$(model_data$, 1) = "usemtl" and len(material_file$) THEN
                current_material_name$ = Word$(model_data$, 2)
                FOR j = 1 TO UBOUND(material)
                    IF RTRIM$(material(j).mtl_name) = current_material_name$ THEN current_mtl = j: EXIT FOR
                NEXT
            END IF
        CASE "v "
            IF v = 1 THEN PRINT: PRINT "Reading Vertices"
            vertices(v).x = VAL(Word$(model_data$, 2))
            vertices(v).y = VAL(Word$(model_data$, 3))
            vertices(v).z = VAL(Word$(model_data$, 4))
            v = v + 1
        CASE "vt"
            IF vt = 1 THEN PRINT: PRINT "Reading Texture Coordinates"
            texture_coordinates(vt).x = VAL(Word$(model_data$, 2))
            texture_coordinates(vt).y = VAL(Word$(model_data$, 3))
            vt = vt + 1
        CASE "vn"
            IF vn = 1 THEN PRINT: PRINT "Reading Normals"
            normals(vn).x = VAL(Word$(model_data$, 2))
            normals(vn).y = VAL(Word$(model_data$, 3))
            normals(vn).z = VAL(Word$(model_data$, 4))
            vn = vn + 1
        CASE "f "
            IF fc = 1 THEN PRINT: PRINT "Reading Faces"
            'vertex data 1
            face_data$ = Word$(model_data$, 2)
                
            k1 = INSTR(1, face_data$, "/")
            k2 = INSTR(k1 + 1, face_data$, "/")
            IF k1 = 0 THEN
                v_index = VAL(face_data$)
            ELSE
                v_index = VAL(MID$(face_data$, 1, k1 - 1))
            END IF
			vt_index = VAL(MID$(face_data$, k1 + 1, k2 - (k1 + 1)))
			
            IF k2 <> 0 THEN vn_index = VAL(RIGHT$(face_data$, LEN(face_data$) - k2))

            'vertex data
            model_faces(fc).v1.v.x = vertices(v_index).x
            model_faces(fc).v1.v.y = vertices(v_index).y
            model_faces(fc).v1.v.z = vertices(v_index).z
            'texture coordinates
            model_faces(fc).v1.vt.x = texture_coordinates(vt_index).x
            model_faces(fc).v1.vt.y = texture_coordinates(vt_index).y
            'and finally, normals
            IF k2 <> 0 THEN
                model_faces(fc).v1.vn.x = normals(vn_index).x
                model_faces(fc).v1.vn.y = normals(vn_index).y
                model_faces(fc).v1.vn.z = normals(vn_index).z
            END IF
            k1 = 0
            k2 = 0
            'vertex data 2
            face_data$ = Word$(model_data$, 3)
                
            k1 = INSTR(1, face_data$, "/")
            k2 = INSTR(k1 + 1, face_data$, "/")
                
            IF k1 = 0 THEN
                v_index = VAL(face_data$)
            ELSE
                v_index = VAL(MID$(face_data$, 1, k1 - 1))
            END IF
                
            vt_index = VAL(MID$(face_data$, k1 + 1, k2 - (k1 + 1)))
            IF k2 <> 0 THEN vn_index = VAL(RIGHT$(face_data$, LEN(face_data$) - k2))
            'vertex data
            model_faces(fc).v2.v.x = vertices(v_index).x
            model_faces(fc).v2.v.y = vertices(v_index).y
            model_faces(fc).v2.v.z = vertices(v_index).z
            'texture coordinates
            model_faces(fc).v2.vt.x = texture_coordinates(vt_index).x
            model_faces(fc).v2.vt.y = texture_coordinates(vt_index).y
            'and finally, normals
            IF k2 <> 0 THEN
                model_faces(fc).v2.vn.x = normals(vn_index).x
                model_faces(fc).v2.vn.y = normals(vn_index).y
                model_faces(fc).v2.vn.z = normals(vn_index).z
            END IF
                
            'vertex data 3
            k1 = 0
            k2 = 0
            face_data$ = Word$(model_data$, 4)
                
            k1 = INSTR(1, face_data$, "/")
            k2 = INSTR(k1 + 1, face_data$, "/")
                
            IF k1 = 0 THEN
                v_index = VAL(face_data$)
            ELSE
                v_index = VAL(MID$(face_data$, 1, k1 - 1))
            END IF
                
            vt_index = VAL(MID$(face_data$, k1 + 1, k2 - (k1 + 1)))
            IF k2 <> 0 THEN vn_index = VAL(RIGHT$(face_data$, LEN(face_data$) - k2))
            'vertex data
            model_faces(fc).v3.v.x = vertices(v_index).x
            model_faces(fc).v3.v.y = vertices(v_index).y
            model_faces(fc).v3.v.z = vertices(v_index).z
            'texture coordinates
            model_faces(fc).v3.vt.x = texture_coordinates(vt_index).x
            model_faces(fc).v3.vt.y = texture_coordinates(vt_index).y
            'and finally, normals
            IF k2 <> 0 THEN
                model_faces(fc).v3.vn.x = normals(vn_index).x
                model_faces(fc).v3.vn.y = normals(vn_index).y
                model_faces(fc).v3.vn.z = normals(vn_index).z
            END IF
                
            '################ Objects and Material #################
            model_faces(fc).obj_name = current_object_name$
                
            IF LEN(material_file$) THEN
                'should be set only if the material file is available
                model_faces(fc).material.mtl_name = current_material_name$
                    
                model_faces(fc).material.ambient.x = material(current_mtl).ambient.x
                model_faces(fc).material.ambient.y = material(current_mtl).ambient.y
                model_faces(fc).material.ambient.z = material(current_mtl).ambient.z
                    
                model_faces(fc).material.diffuse.x = material(current_mtl).diffuse.x
                model_faces(fc).material.diffuse.y = material(current_mtl).diffuse.y
                model_faces(fc).material.diffuse.z = material(current_mtl).diffuse.z
                    
                model_faces(fc).material.specular.x = material(current_mtl).specular.x
                model_faces(fc).material.specular.y = material(current_mtl).specular.y
                model_faces(fc).material.specular.z = material(current_mtl).specular.z
				
                model_faces(fc).material.emission.x = material(current_mtl).emission.x
                model_faces(fc).material.emission.y = material(current_mtl).emission.y
                model_faces(fc).material.emission.z = material(current_mtl).emission.z
                
				model_faces(fc).material.shineness = material(current_mtl).shineness
                model_faces(fc).material.tranparency = material(current_mtl).tranparency
                model_faces(fc).material.texture = material(current_mtl).texture
                    
                model_faces(fc).material.id = material(current_mtl).id
            END IF
                
            model_faces(fc).obj_id = o
                
            fc = fc + 1
    END SELECT
    IF g MOD 200 = 0 THEN PRINT ".";
    g = g + 1
WEND
CLOSE #f
' PRINT "Done. Hit Enter"
' for i = 1 to ubound(model_faces) : print model_faces(i).material.id :_limit 100: next
' a$ = INPUT$(1)
' end
CLS

REDIM SHARED DONT_USE_GLH_Handle(1000) AS DONT_USE_GLH_Handle_TYPE


DIM SHARED mouseX, mouseY, Z_Value, wired_frame, lights_allow
DIM SHARED eye AS vec3, cameraTarget AS vec3

eye.x = 0: eye.y = 2: eye.z = 10
cameraTarget.x = eye.x: cameraTarget.y = eye.y: cameraTarget.z = eye.z - 10

lights_allow = 1
wired_frame = -1

'clear all the keys
_KEYCLEAR

glAllow = -1

DO
    _LIMIT 40
    WHILE _MOUSEINPUT: WEND
    k& = _KEYHIT
    'camera movement
    IF _KEYDOWN(ASC("w")) THEN eye.z = eye.z - .1: cameraTarget.z = eye.z - 5
    IF _KEYDOWN(ASC("s")) THEN eye.z = eye.z + .1: cameraTarget.z = eye.z - 5
    IF _KEYDOWN(ASC("a")) THEN eye.x = eye.x - .1: cameraTarget.x = eye.x
    IF _KEYDOWN(ASC("d")) THEN eye.x = eye.x + .1: cameraTarget.x = eye.x
    IF _KEYDOWN(ASC("q")) THEN eye.y = eye.y + .1: cameraTarget.y = eye.y
    IF _KEYDOWN(ASC("e")) THEN eye.y = eye.y - .1: cameraTarget.y = eye.y
    
    IF k& = ASC(" ") THEN wired_frame = wired_frame * -1
    IF k& = ASC("h") THEN lights_allow = lights_allow * -1
    mouseX = _MOUSEX
    mouseY = _MOUSEY
    CLS , 1
    PRINT "Press 'w' & 's' to move forward & backward"
    PRINT "Press 'a' & 'd' to move left & right"
    PRINT "Pres 'q' & 'e' to move up & down"
    IF wired_frame = -1 THEN
        PRINT "Press space to toggle wire-frame model"
    ELSE
        PRINT "Press space to toggle texture model"
    END IF
    IF lights_allow = -1 THEN
        PRINT "Press 'h' to turn on lights"
    ELSE
        PRINT "Press 'h' to turn off lights"
    END IF
    _DISPLAY
LOOP


SUB _GL () STATIC

    IF NOT glAllow THEN EXIT SUB
    
    IF NOT glSetup THEN
        _glViewport 0, 0, _WIDTH, _HEIGHT
        aspect# = _WIDTH / _HEIGHT
        IF LEN(material_file$) THEN
            FOR i = 1 TO UBOUND(material)
                IF material(i).texture < -1 THEN material(i).gl_tex = GLH_Image_to_Texture(material(i).texture)
            NEXT
        ELSE
            IF LEN(model_texture_file$) THEN
                model_image& = _LOADIMAGE(model_texture_file$)
                model_texture = GLH_Image_to_Texture(model_image&)
                _FREEIMAGE model_image&
            END IF
        END IF
		wire_frame_model_buffer = 0
		textured_model_buffer = 0
        glSetup = -1
    END IF

    _glEnable _GL_DEPTH_TEST

    IF LEN(material_file$) OR LEN(model_texture_file$) THEN
        _glEnable _GL_TEXTURE_2D
    END IF

    IF lights_allow = 1 THEN
        _glEnable _GL_LIGHTING
        _glEnable _GL_LIGHT0

        _glLightfv _GL_LIGHT0, _GL_AMBIENT, glVec3(0.45, 0.45, 0.45)
        _glLightfv _GL_LIGHT0, _GL_DIFFUSE, glVec3(0.6, 0.6, 0.6)
        _glLightfv _GL_LIGHT0, _GL_SPECULAR, glVec3(0.6, 0.6, 0.6)
        _glLightfv _GL_LIGHT0, _GL_POSITION, glVec4(0, 0, 25, 0)
    END IF

    _glMatrixMode _GL_PROJECTION
    _glLoadIdentity
    _gluPerspective 45.0, aspect#, 1.0, 3000.0

    _glMatrixMode _GL_MODELVIEW
    _glLoadIdentity

    gluLookAt eye.x, eye.y, eye.z, cameraTarget.x, cameraTarget.y, cameraTarget.z, 0, 1, 0


    IF wired_frame = -1 THEN
        surface_init = 0
        use_texture_in_opengl = 0
        current_material_id = 0
        current_texture_id = 0
		'######## Code optimize for faster rendering ######
		if done_task1 = 0 then
		    done_task1 = 1
			textured_model_buffer = _glGenLists(1)
			_glNewList textured_model_buffer, _GL_COMPILE
		end if
		_glPushMatrix 'push
		_glRotatef -mouseX * 1.5, 0, 1, 0 'rotate the world
		_glRotatef mouseY * 1.5, 1, 0, 0
        FOR o = 1 TO totalObjects
		if done_task1 = 2 then
		    _glCallList textured_model_buffer
			exit for
		end if
		'#################################################
            FOR i = 1 TO UBOUND(model_faces)
                surface_init = 0
                use_texture_in_opengl = 0
                IF model_faces(i).obj_id = o THEN
                    IF surface_init = 0 THEN
                        surface_init = 1

                        IF LEN(material_file$) THEN
                            IF model_faces(i).material.texture < -1 THEN

                                GLH_Select_Texture material(model_faces(i).material.id).gl_tex
                                _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
                                _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_LINEAR

                                use_texture_in_opengl = -1
                            END IF
							
							'TURN on blending according to our beautiful model needs.
                            if model_faces(i).material.tranparency <> 1 then _glEnable _GL_BLEND else _glDisable _GL_BLEND
							
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_AMBIENT, glVec4(model_faces(i).material.ambient.x, model_faces(i).material.ambient.y, model_faces(i).material.ambient.z, model_faces(i).material.tranparency)
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_DIFFUSE, glVec4(model_faces(i).material.diffuse.x, model_faces(i).material.diffuse.y, model_faces(i).material.diffuse.z, model_faces(i).material.tranparency)
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_SPECULAR, glVec4(model_faces(i).material.specular.x, model_faces(i).material.specular.y, model_faces(i).material.specular.z, model_faces(i).material.tranparency)
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_EMISSION, glVec4(model_faces(i).material.emission.x, model_faces(i).material.emission.y, model_faces(i).material.emission.z, model_faces(i).material.tranparency)
							_glMaterialfv _GL_FRONT_AND_BACK, _GL_SHININESS, glVec3(model_faces(i).material.shineness * 0.128, 0, 0)
                        ELSE
                            IF LEN(model_texture_file$) THEN
                                GLH_Select_Texture model_texture
                                _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
                                _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_LINEAR
                                use_texture_in_opengl = -1
                            END IF
                            'since the material file is not define, we'll setup our own settings.
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_AMBIENT, glVec3(0.45, 0.45, 0.45)
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_DIFFUSE, glVec3(0.977, 0.977, 0.977)
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_SPECULAR, glVec3(.556, .556, .556)
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_SHININESS, glVec3(128 * .3, 0, 0)
                        END IF

                        _glBegin _GL_TRIANGLES
                    END IF

                    current_model_material = model_faces(i).material.id
                    IF LEN(material_file$) THEN
                        IF last_used_material <> current_model_material THEN
                            last_used_material = current_model_material
                            _glEnd

                            IF model_faces(i).material.texture < -1 THEN

                                GLH_Select_Texture material(model_faces(i).material.id).gl_tex
                                _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
                                _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_LINEAR

                                use_texture_in_opengl = -1
                            END IF
                            
                            if model_faces(i).material.tranparency <> 1 then _glEnable _GL_BLEND else _glDisable _GL_BLEND
                            
							_glMaterialfv _GL_FRONT_AND_BACK, _GL_AMBIENT, glVec4(model_faces(i).material.ambient.x, model_faces(i).material.ambient.y, model_faces(i).material.ambient.z, model_faces(i).material.tranparency)
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_DIFFUSE, glVec4(model_faces(i).material.diffuse.x, model_faces(i).material.diffuse.y, model_faces(i).material.diffuse.z, model_faces(i).material.tranparency)
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_SPECULAR, glVec4(model_faces(i).material.specular.x, model_faces(i).material.specular.y, model_faces(i).material.specular.z, model_faces(i).material.tranparency)
							_glMaterialfv _GL_FRONT_AND_BACK, _GL_EMISSION, glVec4(model_faces(i).material.emission.x, model_faces(i).material.emission.y, model_faces(i).material.emission.z, model_faces(i).material.tranparency)
                            _glMaterialfv _GL_FRONT_AND_BACK, _GL_SHININESS, glVec3(model_faces(i).material.shineness * 0.128, 0, 0)

                            _glBegin _GL_TRIANGLES
                        END IF
                    END IF

                    IF use_texture_in_opengl = -1 THEN
                        _glTexCoord2f model_faces(i).v1.vt.x, -model_faces(i).v1.vt.y
                    END IF

                    _glNormal3f model_faces(i).v1.vn.x, model_faces(i).v1.vn.y, model_faces(i).v1.vn.z
                    _glVertex3f model_faces(i).v1.v.x, model_faces(i).v1.v.y, model_faces(i).v1.v.z

                    IF use_texture_in_opengl = -1 THEN
                        _glTexCoord2f model_faces(i).v2.vt.x, -model_faces(i).v2.vt.y
                    END IF
                    _glNormal3f model_faces(i).v2.vn.x, model_faces(i).v2.vn.y, model_faces(i).v2.vn.z
                    _glVertex3f model_faces(i).v2.v.x, model_faces(i).v2.v.y, model_faces(i).v2.v.z
					
					IF use_texture_in_opengl = -1 THEN
                        _glTexCoord2f model_faces(i).v3.vt.x, -model_faces(i).v3.vt.y
                    END IF
                    _glNormal3f model_faces(i).v3.vn.x, model_faces(i).v3.vn.y, model_faces(i).v3.vn.z
                    _glVertex3f model_faces(i).v3.v.x, model_faces(i).v3.v.y, model_faces(i).v3.v.z

                END IF
            NEXT
            _glEnd
			_glPopMatrix
        NEXT
		if done_task1 = 1 then
		    done_task1 = 2
			_glEndList
		end if
    ELSE
	    '######## Code optimize for faster rendering ######
	    if done_task2 = 0 then
		    done_task2 = 1
			wire_frame_model_buffer = _glGenLists(1)
			_glNewList wire_frame_model_buffer, _GL_COMPILE
		end if
		if done_task2 = 2 then
		    _glCallList wire_frame_model_buffer
		end if
		'####################################################
        _glColor3f 1, 1, 1
        FOR i = 1 TO UBOUND(model_faces)
            _glBegin _GL_LINE_LOOP
            _glNormal3f model_faces(i).v1.vn.x, model_faces(i).v1.vn.y, model_faces(i).v1.vn.z
            _glVertex3f model_faces(i).v1.v.x, model_faces(i).v1.v.y, model_faces(i).v1.v.z

            _glNormal3f model_faces(i).v2.vn.x, model_faces(i).v2.vn.y, model_faces(i).v2.vn.z
            _glVertex3f model_faces(i).v2.v.x, model_faces(i).v2.v.y, model_faces(i).v2.v.z

            _glNormal3f model_faces(i).v3.vn.x, model_faces(i).v3.vn.y, model_faces(i).v3.vn.z
            _glVertex3f model_faces(i).v3.v.x, model_faces(i).v3.v.y, model_faces(i).v3.v.z
            _glEnd
        NEXT
		if done_task2 = 1 then
		    _glEndList
		end if
    END IF

    _glFlush

    clock# = clock# + .01
END SUB

FUNCTION glVec3%& (x, y, z)
    STATIC internal_vec3(2)
    internal_vec3(0) = x
    internal_vec3(1) = y
    internal_vec3(2) = z
    glVec3%& = _OFFSET(internal_vec3())
END FUNCTION

FUNCTION glVec4%& (x, y, z, w)
    STATIC internal_vec4(3)
    internal_vec4(0) = x
    internal_vec4(1) = y
    internal_vec4(2) = z
    internal_vec4(3) = w
    glVec4%& = _OFFSET(internal_vec4())
END FUNCTION

FUNCTION map! (value!, minRange!, maxRange!, newMinRange!, newMaxRange!)
    map! = ((value! - minRange!) / (maxRange! - minRange!)) * (newMaxRange! - newMinRange!) + newMinRange!
END FUNCTION

$checking:off
'below, all functions are coded by Galleon
FUNCTION GLH_Image_to_Texture (image_handle AS LONG) 'turn an image handle into a texture handle
    IF image_handle >= 0 THEN ERROR 258: EXIT FUNCTION 'don't allow screen pages
    DIM m AS _MEM
    m = _MEMIMAGE(image_handle)
    DIM h AS LONG
    h = DONT_USE_GLH_New_Texture_Handle
    GLH_Image_to_Texture = h
    _glBindTexture _GL_TEXTURE_2D, DONT_USE_GLH_Handle(h).handle
    _glTexImage2D _GL_TEXTURE_2D, 0, _GL_RGBA, _WIDTH(image_handle), _HEIGHT(image_handle), 0, &H80E1&&, _GL_UNSIGNED_BYTE, m.OFFSET
    _MEMFREE m
END FUNCTION

FUNCTION DONT_USE_GLH_New_Texture_Handle
    handle&& = 0
    _glGenTextures 1, _OFFSET(handle&&)
    DONT_USE_GLH_New_Texture_Handle = handle&&
    FOR h = 1 TO UBOUND(DONT_USE_GLH_Handle)
        IF DONT_USE_GLH_Handle(h).in_use = 0 THEN
            DONT_USE_GLH_Handle(h).in_use = 1
            DONT_USE_GLH_Handle(h).handle = handle&&
            DONT_USE_GLH_New_Texture_Handle = h
            EXIT FUNCTION
        END IF
    NEXT
    REDIM _PRESERVE DONT_USE_GLH_Handle(UBOUND(DONT_USE_GLH_HANDLE) * 2) AS DONT_USE_GLH_Handle_TYPE
    DONT_USE_GLH_Handle(h).in_use = 1
    DONT_USE_GLH_Handle(h).handle = handle&&
    DONT_USE_GLH_New_Texture_Handle = h
END FUNCTION

SUB GLH_Select_Texture (texture_handle AS LONG) 'turn an image handle into a texture handle
    IF texture_handle < 1 OR texture_handle > UBOUND(DONT_USE_GLH_HANDLE) THEN ERROR 258: EXIT FUNCTION
    IF DONT_USE_GLH_Handle(texture_handle).in_use = 0 THEN ERROR 258: EXIT FUNCTION
    _glBindTexture _GL_TEXTURE_2D, DONT_USE_GLH_Handle(texture_handle).handle
END SUB

'By Tyler Darko
'Modified By : Steve McNeill
FUNCTION Word$ (__list$, index AS LONG)
    DIM list$, idx AS LONG, i AS LONG, nextSpace AS LONG

    'fool proof input
    list$ = LTRIM$(RTRIM$(__list$)) + " " 'add a space to the end to search for
    IF list$ = " " THEN Word$ = "ERROR: NO WORDS IN LIST": EXIT FUNCTION 'it was blank, or all spaces, until we added a space.  Manual Error, so we can parse and process for it in our routines if wanted.
    IF index <= 0 THEN Word$ = "ERROR: INVALID WORD COUNT TO SEARCH FOR (" + LTRIM$(STR$(index)) + ")": EXIT FUNCTION 'Can't search for negative words or zero words in our list.  Give the user an error.

    'remove multiple spaces between words
    DO WHILE INSTR(list$, "  ")
        i = INSTR(list$, "  ")
        list$ = LEFT$(list$, i - 1) + MID$(list$, i + 1)
    LOOP

    'read up to the desired index and return the desired word
    DO
        idx = idx + 1
        startPosition = nextSpace + 1 'the next word starts at the end of the last space
        nextSpace = INSTR(startPosition, list$, " ") 'look for the space after the next word
        IF idx = index THEN Word$ = MID$(list$, startPosition, nextSpace - startPosition): EXIT FUNCTION 'once we get to the proper word, we extract it from the list and be done with it.
    LOOP UNTIL nextSpace + 1 >= LEN(list$) 'that space we added at the end will be found, telling us when we finished searching the list
    Word$ = "ERROR: NOT ENOUGH WORDS IN LIST (" + LTRIM$(STR$(index)) + " of" + STR$(idx) + ")" 'Our list didn't have enough words for the user's search.  Give them an error.
END FUNCTION


'QB64 Rocks!! :O
