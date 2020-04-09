_TITLE "Mesh Animation Demo"

SCREEN _NEWIMAGE(600, 600, 32)

declare library
	sub glutSolidCube (byval Dsize as single)
end declare

REDIM SHARED myMesh(0) AS SINGLE, myMeshInfo(0) AS mesh_part_info, objFileInfo AS MDL_INFO

DIM SHARED glAllow

' objLoad "heli.obj", myMesh(), myMeshInfo(), objFileInfo
' objExportBin "heli.binobj", myMesh(), myMeshInfo(), objFileInfo

objImportBin "heli.binobj", myMesh(), myMeshInfo(), objFileInfo
objHideMeshPart "main_rotor_fan", myMeshInfo()
objHideMeshPart "back_heli_rotor", myMeshInfo()

PRINT "No. of materials : "; objFileInfo.num_of_materials

_GLRENDER _BEHIND
glAllow = -1

DIM SHARED worldRotX, worldRotY, scaleFactor
scaleFactor = 4
mode = 1
light = 1
_KEYCLEAR
DO
    WHILE _MOUSEINPUT
        scaleFactor = scaleFactor + _MOUSEWHEEL * 0.1
    WEND
    IF _MOUSEBUTTON(1) THEN
        dx = _MOUSEX: dy = _MOUSEY
        WHILE _MOUSEBUTTON(1)
            WHILE _MOUSEINPUT: WEND
            worldRotX = worldRotX + (_MOUSEY - dy)
            worldRotY = worldRotY + (_MOUSEX - dx)
            dy = _MOUSEY
            dx = _MOUSEX
        WEND
    END IF

    _LIMIT 30
LOOP


SUB _GL ()
    STATIC init, yrot, xrot
    IF NOT glAllow THEN EXIT SUB
    _glEnable _GL_LIGHTING
    _glEnable _GL_LIGHT0
    _glEnable _GL_DEPTH_TEST
    _glEnable _GL_TEXTURE_2D
	
	
	_glClearColor 0.3,0.3,0.3,1
	_glClear _GL_COLOR_BUFFER_BIT
	
    _glMatrixMode _GL_PROJECTION
    _gluPerspective 50, 1, 0.1, 100

    _glMatrixMode _GL_MODELVIEW
    _glTranslatef 0, 0, -4

    IF init = 0 THEN
        init = 1
        objInit myMeshInfo()
    END IF
	_glRotatef worldRotX,1,0,0
	_glRotatef worldRotY,0,1,0
	
	'drawing the main rotor with rotation
	_glPushMatrix
    yrot = yrot + 20
	setMeshPartOrigin "main_rotor_fan", myMeshInfo(), myMesh()
    _glRotatef yrot, 0, 1, 0
	objDrawMeshPart "main_rotor_fan", myMesh(), myMeshInfo(), objFileInfo
	_glPopMatrix
	
	'drawing the back rotor with rotation
	_glPushMatrix
	xrot = xrot + 20
	setMeshPartOrigin "back_heli_rotor", myMeshInfo(), myMesh()
	_glRotatef xrot,1,0,0
	objDrawMeshPart "back_heli_rotor", myMesh(), myMeshInfo(), objFileInfo
	_glPopMatrix
	
	'drawing the rest of the stuff
    objDraw myMesh(), myMeshInfo(), objFileInfo
    
END SUB

'$INCLUDE:'obj_loader_LIB.bas'