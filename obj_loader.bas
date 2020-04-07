'#################################################################
'         OpenGL OBJ Loader In QB64
'                v2.0
'          By Ashish Kushwaha
'----------------------------------------------------------------
'Improved model loading speed.
'use commandline - obj_loader [obj_file_name] OR set f$ to your
'OBJ file name.
'On Windows, you can simply drag the OBJ file on the executable
'and it will load the model.
'----------------------------------------------------------------
'Controls:- Use 'L' to toggle light
' Drag with mouse anywhere for the rotation.
' Use spacebar to switch between rendering options.
'

_TITLE "OBJ Loader v2.0"
SCREEN _NEWIMAGE(1000, 700, 32)

TYPE vec3
    x AS SINGLE
    y AS SINGLE
    z AS SINGLE
END TYPE

TYPE material_info
    exits AS _BYTE 'material exits or not
    init AS _BYTE
    id AS STRING * 128 'material name
    amb AS vec3 'ambient
    diff AS vec3 'diffuse
    spec AS vec3 'specular
    emis AS vec3 'emission
    img_tex AS LONG 'image handle
    gl_tex AS LONG 'GL tex handle
    shine AS SINGLE 'shineness
    trans AS SINGLE 'transparency
END TYPE

TYPE mesh_part_info
    start AS _UNSIGNED LONG 'start index of mesh()
    length AS _UNSIGNED LONG 'length
    init AS _BYTE 'intialize?
    mtl AS material_info 'material properties
END TYPE

DECLARE LIBRARY 'camera control function
    SUB gluLookAt (BYVAL eyeX#, BYVAL eyeY#, BYVAL eyeZ#, BYVAL centerX#, BYVAL centerY#, BYVAL centerZ#, BYVAL upX#, BYVAL upY#, BYVAL upZ#)
END DECLARE


f$ = "models/pyramid.obj"
IF COMMAND$(1) <> "" THEN f$ = COMMAND$(1) 'load the file if passed from commandline
'seprate file name and path
x = _INSTRREV(f$, "/") + _INSTRREV(f$, "\")
IF x = 0 THEN
    obj_file$ = f$
ELSE
    obj_file$ = RIGHT$(f$, LEN(f$) - x)
    path$ = LEFT$(f$, x)
END IF

DIM tag(4) AS STRING * 3, p(4) AS _UNSIGNED LONG 'tag() contain keywords like v, vt, etc. p() store the position for each keywords independently
DIM SHARED v(4) AS _UNSIGNED LONG 'v(0) -> no. of vertices, v(1)->no. of tex. coord.,v(2)->no. of normals, v(3)->no. of faces
REDIM vert(2) AS SINGLE, norm(2) AS SINGLE, texcoord(1) AS SINGLE 'vert(), norm() and texcoord() will store all the vertices, normals and texture coordinates()
REDIM SHARED mesh(23) AS SINGLE '(3 vert + 2 tex coord + 3 norm )* 3 vert of triangle : This is the main data which we will pass to OpenGL
REDIM SHARED materials(0) AS material_info, mesh_part(0) AS mesh_part_info 'contain properties of mesh like materials
DIM SHARED materialPresent
DIM SHARED glAllow

tag(0) = CHR$(10) + "v ": tag(1) = CHR$(10) + "vt": tag(2) = CHR$(10) + "vn": tag(3) = CHR$(10) + "f ": tag(4) = CHR$(10) + "o "
DIM LINE_FEED AS STRING * 1
LINE_FEED = CHR$(10)
OPEN f$ FOR BINARY AS #1
length = LOF(1)
a$ = SPACE$(length)
GET #1, , a$
CLOSE #1

'check if the mtl file exits for the given OBJ
x = INSTR(1, a$, LINE_FEED + "mtllib")
IF x THEN 'yes it exits
    FOR i = x + 8 TO LEN(a$)
        IF MID$(a$, i, 1) = CHR$(13) OR MID$(a$, i, 1) = LINE_FEED THEN y = i: EXIT FOR
    NEXT
    mtl_file$ = path$ + _TRIM$(MID$(a$, x + 8, y - (x + 8)))
    IF NOT _FILEEXISTS(mtl_file$) THEN PRINT "ERROR : File not found - " + mtl_file$: END
    x = 1
    materialPresent = 1
    mtl_index = -1 'so it start with 0 in the loop
    OPEN mtl_file$ FOR INPUT AS #2
    WHILE NOT EOF(2)
        LINE INPUT #2, b$
        IF LEFT$(b$, 1) <> "#" THEN 'to avoid comments. Comments in OBJ/MTL start with #
            IF LEFT$(b$, 6) = "newmtl" THEN 'new material
                mtl_index = mtl_index + 1
                REDIM _PRESERVE materials(mtl_index) AS material_info
                materials(mtl_index).id = _TRIM$(MID$(b$, 7, LEN(b$) - 6))
                materials(mtl_index).exits = 1
            ELSEIF LEFT$(b$, 2) = "Ka" THEN 'ambient
                y1 = INSTR(4, b$, " ")
                y2 = INSTR(y1 + 1, b$, " ")
                materials(mtl_index).amb.x = VAL(MID$(b$, 4, y1 - 3))
                materials(mtl_index).amb.y = VAL(MID$(b$, y1, y2 - y1 + 1))
                materials(mtl_index).amb.z = VAL(RIGHT$(b$, LEN(b$) - y2 + 1))
            ELSEIF LEFT$(b$, 2) = "Kd" THEN 'diffuse
                y1 = INSTR(4, b$, " ")
                y2 = INSTR(y1 + 1, b$, " ")
                materials(mtl_index).diff.x = VAL(MID$(b$, 4, y1 - 3))
                materials(mtl_index).diff.y = VAL(MID$(b$, y1, y2 - y1 + 1))
                materials(mtl_index).diff.z = VAL(RIGHT$(b$, LEN(b$) - y2 + 1))
            ELSEIF LEFT$(b$, 2) = "Ks" THEN 'specular
                y1 = INSTR(4, b$, " ")
                y2 = INSTR(y1 + 1, b$, " ")
                materials(mtl_index).spec.x = VAL(MID$(b$, 4, y1 - 3))
                materials(mtl_index).spec.y = VAL(MID$(b$, y1, y2 - y1 + 1))
                materials(mtl_index).spec.z = VAL(RIGHT$(b$, LEN(b$) - y2 + 1))
            ELSEIF LEFT$(b$, 2) = "Ke" THEN 'emission
                y1 = INSTR(4, b$, " ")
                y2 = INSTR(y1 + 1, b$, " ")
                materials(mtl_index).emis.x = VAL(MID$(b$, 4, y1 - 3))
                materials(mtl_index).emis.y = VAL(MID$(b$, y1, y2 - y1 + 1))
                materials(mtl_index).emis.z = VAL(RIGHT$(b$, LEN(b$) - y2 + 1))
            ELSEIF LEFT$(b$, 2) = "Ns" THEN 'shineness
                materials(mtl_index).shine = VAL(MID$(b$, 3, LEN(b$) - 2))
            ELSEIF LEFT$(b$, 2) = "d " THEN 'transparency
                materials(mtl_index).trans = VAL(MID$(b$, 2, LEN(b$) - 1))
            ELSEIF LEFT$(b$, 6) = "map_Kd" THEN 'texture file name
                img_file$ = path$ + _TRIM$(MID$(b$, 7, LEN(b$) - 6))
                dummy_img& = _LOADIMAGE(img_file$)
                IF NOT dummy_img& < -1 THEN PRINT "ERROR : Could not load the texture - " + img_file$: END
                materials(mtl_index).img_tex = dummy_img&
            END IF
        END IF
    WEND
    CLOSE #2
END IF

t = TIMER
PRINT "Loading... - " + f$
' $CHECKING:OFF
p(0) = 1: p(1) = 1: p(2) = 1: p(3) = 1: p(4) = 1
'get position of first mention of material to be used
mtl_first = INSTR(1, a$, LINE_FEED + "usemtl"): mtl_second = INSTR(mtl_first + 1, a$, LINE_FEED + "usemtl")
mtl_id$ = _TRIM$(MID$(a$, mtl_first + 8, INSTR(mtl_first + 8, a$, LINE_FEED) - (mtl_first + 8)))
FOR j = 0 TO UBOUND(materials)
    IF RTRIM$(materials(j).id) = mtl_id$ THEN mtl_index = j: EXIT FOR
NEXT
'This is the main loop of reading the file. It does all the things which is required.
DO
    x = INSTR(p(c), a$, tag(c))
    IF x > 0 THEN
        v(c) = v(c) + 1
        IF c = 0 THEN 'store vertices
            y1 = INSTR(x + 3, a$, " ")
            y2 = INSTR(y1 + 1, a$, " ")
            y3 = INSTR(y2 + 1, a$, LINE_FEED)
            vert(v_index) = VAL(MID$(a$, x + 3, y1 - (x + 3)))
            vert(v_index + 1) = VAL(MID$(a$, y1, y2 - y1))
            vert(v_index + 2) = VAL(MID$(a$, y2, y3 - y2))
            v_index = v_index + 3
            REDIM _PRESERVE vert(UBOUND(vert) + 3) AS SINGLE
        ELSEIF c = 1 THEN 'store tex coord.
            y1 = INSTR(x + 4, a$, " ")
            y2 = INSTR(y1 + 1, a$, LINE_FEED)
            texcoord(vt_index) = VAL(MID$(a$, x + 4, y1 - (x + 4)))
            texcoord(vt_index + 1) = -VAL(MID$(a$, y1, y2 - y1))
            vt_index = vt_index + 2
            REDIM _PRESERVE texcoord(UBOUND(texcoord) + 2) AS SINGLE
        ELSEIF c = 2 THEN 'store normals
            y1 = INSTR(x + 4, a$, " ")
            y2 = INSTR(y1 + 1, a$, " ")
            y3 = INSTR(y2 + 1, a$, LINE_FEED)
            norm(vn_index) = VAL(MID$(a$, x + 4, y1 - (x + 4)))
            norm(vn_index + 1) = VAL(MID$(a$, y1, y2 - y1))
            norm(vn_index + 2) = VAL(MID$(a$, y2, y3 - y2))
            vn_index = vn_index + 3
            REDIM _PRESERVE norm(UBOUND(norm) + 3) AS SINGLE
        ELSEIF c = 3 THEN 'face part
            'check if there is new material to be used for face. If not then array length for current mesh_info() increases
            check_for_obj:
            IF x > mtl_first AND x < mtl_second OR mtl_second = 0 THEN
                IF mesh_part(mp_index).init = 0 THEN mesh_part(mp_index).init = 1: mesh_part(mp_index).start = m_index / 8: mesh_part(mp_index).mtl = materials(mtl_index)
                mesh_part(mp_index).length = mesh_part(mp_index).length + 3 '3 vertex in a face
            ELSE
                mtl_first = mtl_second
                mtl_second = INSTR(mtl_first + 1, a$, LINE_FEED + "usemtl")
                mtl_id$ = _TRIM$(MID$(a$, mtl_first + 8, INSTR(mtl_first + 8, a$, LINE_FEED) - (mtl_first + 8)))
                FOR j = 0 TO UBOUND(materials)
                    IF RTRIM$(materials(j).id) = mtl_id$ THEN mtl_index = j: EXIT FOR
                NEXT
                mp_index = mp_index + 1
                REDIM _PRESERVE mesh_part(mp_index) AS mesh_part_info
                GOTO check_for_obj
            END IF
            'reading of face data comes heer
            spc_1 = x + 2: spc_2 = INSTR(spc_1 + 1, a$, " ")
            y_max = INSTR(x + 1, a$, LINE_FEED)

            n = -1 'so start with 0 in while loop
            'get the each vertex info block which are sperated by space
            REDIM dat(0) AS STRING
            WHILE 1 'spc_2<y_max
                n = n + 1
                REDIM _PRESERVE dat(n) AS STRING
                IF spc_2 >= y_max OR spc_2 < spc_1 THEN
                    dat(n) = MID$(a$, spc_1 + 1, y_max - (spc_1 + 1))
                    EXIT WHILE
                ELSE
                    dat(n) = MID$(a$, spc_1 + 1, spc_2 - (spc_1 + 1))
                END IF
                SWAP spc_1, spc_2
                spc_2 = INSTR(spc_1 + 1, a$, " ")
            WEND

            REDIM z_ref(n, 1) AS INTEGER
            'get ref info for v,vt,vn
            FOR i = 0 TO n
                z_ref(i, 0) = INSTR(1, dat(i), "/")
                z_ref(i, 1) = _INSTRREV(dat(i), "/")
            NEXT

            IF z_ref(0, 0) = 0 THEN 'only v data given
                IF (n + 1) > 3 THEN 'polygon face given : So, subdivide the polygon in minimum no. of triangles
                    v_r1 = (VAL(dat(0)) - 1) * 3
                    FOR i = 3 TO (n + 1)
                        v_r2 = (VAL(dat(i - 2)) - 1) * 3
                        v_r3 = (VAL(dat(i - 1)) - 1) * 3
                        mesh(m_index) = vert(v_r1): mesh(m_index + 1) = vert(v_r1 + 1): mesh(m_index + 2) = vert(v_r1 + 2)
                        m_index = m_index + 8
                        mesh(m_index) = vert(v_r2): mesh(m_index + 1) = vert(v_r2 + 1): mesh(m_index + 2) = vert(v_r2 + 2)
                        m_index = m_index + 8
                        mesh(m_index) = vert(v_r3): mesh(m_index + 1) = vert(v_r3 + 1): mesh(m_index + 2) = vert(v_r3 + 2)
                        m_index = m_index + 8
                        REDIM _PRESERVE mesh(UBOUND(mesh) + 24) AS SINGLE
                    NEXT
                    'add additional new vertices
                    mesh_part(mp_index).length = mesh_part(mp_index).length + 3 * (n - 2) '3*(X - 2) - 3 => 3*(X - 3), now here X = n+1, so, 3*(n-2)
                ELSE 'simply a triangle face
                    FOR j = 0 TO 2
                        v_r1 = (VAL(dat(j)) - 1) * 3
                        mesh(m_index) = vert(v_r1): mesh(m_index + 1) = vert(v_r1 + 1): mesh(m_index + 2) = vert(v_r1 + 2)
                        m_index = m_index + 8
                    NEXT
                    REDIM _PRESERVE mesh(UBOUND(mesh) + 24) AS SINGLE
                END IF
            ELSEIF z_ref(0, 1) - z_ref(0, 0) = 1 THEN 'only v and vn data given
                IF (n + 1) > 3 THEN
                    v_r1 = (VAL(LEFT$(dat(0), z_ref(0, 0))) - 1) * 3
                    vn_r1 = (VAL(RIGHT$(dat(0), LEN(dat(0)) - z_ref(0, 1))) - 1) * 3
                    FOR i = 3 TO (n + 1)
                        v_r2 = (VAL(LEFT$(dat(i - 2), z_ref(i - 2, 0))) - 1) * 3: vn_r2 = (VAL(RIGHT$(dat(i - 2), LEN(dat(i - 2)) - z_ref((i - 2), 1))) - 1) * 3
                        v_r3 = (VAL(LEFT$(dat(i - 1), z_ref(i - 1, 0))) - 1) * 3: vn_r3 = (VAL(RIGHT$(dat(i - 1), LEN(dat(i - 1)) - z_ref((i - 1), 1))) - 1) * 3

                        mesh(m_index) = vert(v_r1): mesh(m_index + 1) = vert(v_r1 + 1): mesh(m_index + 2) = vert(v_r1 + 2)
                        mesh(m_index + 5) = norm(vn_r1): mesh(m_index + 6) = norm(vn_r1 + 1): mesh(m_index + 7) = norm(vn_r1 + 2)
                        m_index = m_index + 8
                        mesh(m_index) = vert(v_r2): mesh(m_index + 1) = vert(v_r2 + 1): mesh(m_index + 2) = vert(v_r2 + 2)
                        mesh(m_index + 5) = norm(vn_r2): mesh(m_index + 6) = norm(vn_r2 + 1): mesh(m_index + 7) = norm(vn_r2 + 2)
                        m_index = m_index + 8
                        mesh(m_index) = vert(v_r3): mesh(m_index + 1) = vert(v_r3 + 1): mesh(m_index + 2) = vert(v_r3 + 2)
                        mesh(m_index + 5) = norm(vn_r3): mesh(m_index + 6) = norm(vn_r3 + 1): mesh(m_index + 7) = norm(vn_r3 + 2)
                        m_index = m_index + 8
                        REDIM _PRESERVE mesh(UBOUND(mesh) + 24) AS SINGLE
                    NEXT
                    'add additional new vertices
                    mesh_part(mp_index).length = mesh_part(mp_index).length + 3 * (n - 2) '3*(X - 2) - 3 => 3*(X - 3), now here X = n+1, so, 3*(n-2)
                ELSE
                    FOR j = 0 TO 2
                        v_r1 = (VAL(LEFT$(dat(j), z_ref(j, 0) - 1)) - 1) * 3: vn_r1 = (VAL(RIGHT$(dat(j), LEN(dat(j)) - z_ref(j, 1))) - 1) * 3
                        'v
                        mesh(m_index) = vert(v_r1): mesh(m_index + 1) = vert(v_r1 + 1): mesh(m_index + 2) = vert(v_r1 + 2)
                        'vn
                        mesh(m_index + 5) = norm(vn_r1): mesh(m_index + 6) = norm(vn_r1 + 1): mesh(m_index + 7) = norm(vn_r1 + 2)
                        m_index = m_index + 8
                    NEXT
                    REDIM _PRESERVE mesh(UBOUND(mesh) + 24) AS SINGLE
                END IF
            ELSE 'v, vn and vt data given
                IF (n + 1) > 3 THEN
                    'reference for first vertex of a polygon
                    v_r1 = (VAL(LEFT$(dat(0), z_ref(0, 0))) - 1) * 3
                    vn_r1 = (VAL(RIGHT$(dat(0), LEN(dat(0)) - z_ref(0, 1))) - 1) * 3
                    vt_r1 = (VAL(MID$(dat(0), z_ref(0, 0) + 1, z_ref(0, 1) - (z_ref(0, 0) + 1))) - 1) * 2
                    FOR i = 3 TO (n + 1)
                        v_r2 = (VAL(LEFT$(dat(i - 2), z_ref(i - 2, 0))) - 1) * 3: vn_r2 = (VAL(RIGHT$(dat(i - 2), LEN(dat(i - 2)) - z_ref((i - 2), 1))) - 1) * 3: vt_r2 = (VAL(MID$(dat(i - 2), z_ref(i - 2, 0) + 1, z_ref(i - 2, 1) - (z_ref(i - 2, 0) + 1))) - 1) * 2
                        v_r3 = (VAL(LEFT$(dat(i - 1), z_ref(i - 1, 0))) - 1) * 3: vn_r3 = (VAL(RIGHT$(dat(i - 1), LEN(dat(i - 1)) - z_ref((i - 1), 1))) - 1) * 3: vt_r3 = (VAL(MID$(dat(i - 1), z_ref(i - 1, 0) + 1, z_ref(i - 1, 1) - (z_ref(i - 1, 0) + 1))) - 1) * 2
                        mesh(m_index) = vert(v_r1): mesh(m_index + 1) = vert(v_r1 + 1): mesh(m_index + 2) = vert(v_r1 + 2)
                        mesh(m_index + 3) = texcoord(vt_r1): mesh(m_index + 4) = texcoord(vt_r1 + 1)
                        mesh(m_index + 5) = norm(vn_r1): mesh(m_index + 6) = norm(vn_r1 + 1): mesh(m_index + 7) = norm(vn_r1 + 2)
                        m_index = m_index + 8
                        mesh(m_index) = vert(v_r2): mesh(m_index + 1) = vert(v_r2 + 1): mesh(m_index + 2) = vert(v_r2 + 2)
                        mesh(m_index + 3) = texcoord(vt_r2): mesh(m_index + 4) = texcoord(vt_r2 + 1)
                        mesh(m_index + 5) = norm(vn_r2): mesh(m_index + 6) = norm(vn_r2 + 1): mesh(m_index + 7) = norm(vn_r2 + 2)
                        m_index = m_index + 8
                        mesh(m_index) = vert(v_r3): mesh(m_index + 1) = vert(v_r3 + 1): mesh(m_index + 2) = vert(v_r3 + 2)
                        mesh(m_index + 3) = texcoord(vt_r3): mesh(m_index + 4) = texcoord(vt_r3 + 1)
                        mesh(m_index + 5) = norm(vn_r3): mesh(m_index + 6) = norm(vn_r3 + 1): mesh(m_index + 7) = norm(vn_r3 + 2)
                        m_index = m_index + 8
                        REDIM _PRESERVE mesh(UBOUND(mesh) + 24) AS SINGLE
                    NEXT
                    mesh_part(mp_index).length = mesh_part(mp_index).length + 3 * (n - 2) '3*(X - 2) - 3 => 3*(X - 3), now here X = n+1, so, 3*(n-2)
                ELSE
                    FOR j = 0 TO 2
                        v_r1 = (VAL(LEFT$(dat(j), z_ref(j, 0) - 1)) - 1) * 3: vn_r1 = (VAL(RIGHT$(dat(j), LEN(dat(j)) - z_ref(j, 1))) - 1) * 3: vt_r1 = (VAL(MID$(dat(j), z_ref(j, 0) + 1, z_ref(j, 1) - (z_ref(j, 0) + 1))) - 1) * 2
                        'v
                        mesh(m_index) = vert(v_r1): mesh(m_index + 1) = vert(v_r1 + 1): mesh(m_index + 2) = vert(v_r1 + 2)
                        'vt
                        mesh(m_index + 3) = texcoord(vt_r1): mesh(m_index + 4) = texcoord(vt_r1 + 1)
                        'vn
                        mesh(m_index + 5) = norm(vn_r1): mesh(m_index + 6) = norm(vn_r1 + 1): mesh(m_index + 7) = norm(vn_r1 + 2)
                        m_index = m_index + 8
                    NEXT
                    REDIM _PRESERVE mesh(UBOUND(mesh) + 24) AS SINGLE
                END IF
            END IF

            ERASE dat, z_ref
        END IF
        p(c) = x + 1
    ELSE
        IF c = 4 THEN EXIT DO ELSE c = c + 1
    END IF
    x = 0
LOOP
' $CHECKING:ON
PRINT TIMER - t; "s"
PRINT "No. of vertices : "; v(0)
PRINT "No. of tex coord : "; v(1)
PRINT "No. of normals : "; v(2)
PRINT "No. of objects : "; v(4)
PRINT "No. of faces : "; v(3)
PRINT "No. of materials : "; UBOUND(materials) + 1
PRINT "Hit ENTER"
SLEEP
ERASE vert, norm, texcoord 'not require now. So save memory whenever possible.
a$ = ""

DIM SHARED worldRotX, worldRotY, scaleFactor, mode, light
scaleFactor = 4
mode = 1
light = 1
_KEYCLEAR
glAllow = 1
DO
    k& = _KEYHIT
    IF k& = ASC(" ") THEN mode = mode * -1
    IF k& = ASC("l") OR k& = ASC("L") THEN light = light * -1
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
    STATIC aspect, init
    IF glAllow = 0 THEN EXIT SUB

    IF init = 0 THEN 'load all the textures if the model uses any
        FOR i = 0 TO UBOUND(mesh_part)
            IF mesh_part(i).mtl.img_tex < -1 THEN mesh_part(i).mtl.gl_tex = feedGLTexture(mesh_part(i).mtl.img_tex)
        NEXT
        init = 1
        aspect = _WIDTH / _HEIGHT
    END IF
    
    _glClearColor 0.49, 0.49, 0.49, 1.0
    _glClear _GL_DEPTH_BUFFER_BIT OR _GL_COLOR_BUFFER_BIT
    
    _glDisable _GL_MULTISAMPLE 'just to increase a little bit speed. Comment this line, you will get much better view

    _glEnable _GL_DEPTH_TEST
    IF light = 1 THEN
        _glEnable _GL_LIGHTING
        _glEnable _GL_LIGHT0

        _glLightfv _GL_LIGHT0, _GL_AMBIENT, glVec4(0.0, 0.0, 0.0, 1)
        _glLightfv _GL_LIGHT0, _GL_DIFFUSE, glVec4(1.0, 1.0, 1.0, 1)
        _glLightfv _GL_LIGHT0, _GL_SPECULAR, glVec4(0.6, 0.6, 0.6, 1)
        _glLightfv _GL_LIGHT0, _GL_POSITION, glVec4(0, 0, 25, 0)
    END IF
    _glEnable _GL_BLEND
    _glEnable _GL_TEXTURE_2D

    _glMatrixMode _GL_PROJECTION
    _gluPerspective 50, aspect, 0.1, 100

    _glMatrixMode _GL_MODELVIEW
    _glLoadIdentity

    gluLookAt 0, 0, scaleFactor, 0, 0, 0, 0, 1, 0

    _glRotatef worldRotX, 1, 0, 0
    _glRotatef worldRotY, 0, 1, 0

    FOR i = 0 TO UBOUND(mesh_part) 'draw the mesh

        IF materialPresent = 1 THEN
            IF mesh_part(i).mtl.trans = 1 THEN _glDisable _GL_BLEND ELSE _glEnable _GL_BLEND
            _glMaterialfv _GL_FRONT, _GL_AMBIENT, glVec4(mesh_part(i).mtl.amb.x, mesh_part(i).mtl.amb.y, mesh_part(i).mtl.amb.z, mesh_part(i).mtl.trans)
            _glMaterialfv _GL_FRONT, _GL_DIFFUSE, glVec4(mesh_part(i).mtl.diff.x, mesh_part(i).mtl.diff.y, mesh_part(i).mtl.diff.z, mesh_part(i).mtl.trans)
            _glMaterialfv _GL_FRONT, _GL_SPECULAR, glVec4(mesh_part(i).mtl.spec.x, mesh_part(i).mtl.spec.y, mesh_part(i).mtl.spec.z, mesh_part(i).mtl.trans)
            _glMaterialfv _GL_FRONT, _GL_EMISSION, glVec4(mesh_part(i).mtl.emis.x, mesh_part(i).mtl.emis.y, mesh_part(i).mtl.emis.z, mesh_part(i).mtl.trans)
            _glMaterialfv _GL_FRONT, _GL_SHININESS, glVec4(mesh_part(i).mtl.shine * 0.128, 0, 0, 0)
        END IF
        _glEnableClientState _GL_VERTEX_ARRAY
        _glVertexPointer 3, _GL_FLOAT, 32, _OFFSET(mesh()) + 32 * mesh_part(i).start
        IF v(1) > 0 THEN
            selectTexture mesh_part(i).mtl.gl_tex
            _glEnableClientState _GL_TEXTURE_COORD_ARRAY
            _glTexCoordPointer 3, _GL_FLOAT, 32, _OFFSET(mesh()) + 12 + 32 * mesh_part(i).start
        END IF
        IF v(2) > 0 THEN
            _glEnableClientState _GL_NORMAL_ARRAY
            _glNormalPointer _GL_FLOAT, 32, _OFFSET(mesh()) + 20 + 32 * mesh_part(i).start
        END IF
        IF mode = 1 THEN _glDrawArrays _GL_TRIANGLES, 0, mesh_part(i).length ELSE _glDrawArrays _GL_LINES, 0, mesh_part(i).length
    NEXT
    _glDisableClientState _GL_VERTEX_ARRAY
    _glDisableClientState _GL_NORMAL_ARRAY
    _glDisableClientState _GL_TEXTURE_COORD_ARRAY
END SUB

'################# Internal Functions ##################################

FUNCTION feedGLTexture& (img AS LONG)
    IF img < -1 THEN
        DIM m AS _MEM
        m = _MEMIMAGE(img)

        _glGenTextures 1, _OFFSET(feedGLTexture&)
        _glBindTexture _GL_TEXTURE_2D, feedGLTexture&

        _glTexImage2D _GL_TEXTURE_2D, 0, _GL_RGBA, _WIDTH(img&), _HEIGHT(img&), 0, _GL_BGRA_EXT, _GL_UNSIGNED_BYTE, m.OFFSET

        _MEMFREE m
        _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_LINEAR
        _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST

    ELSE
        PRINT "FUNCTION feedGlTexture&() : invalid image handle passed"
    END IF
END FUNCTION

SUB selectTexture (tex&)
    _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_LINEAR
    _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_LINEAR
    _glBindTexture _GL_TEXTURE_2D, tex&
END SUB

FUNCTION glVec4%& (x, y, z, w)
    STATIC internal_vec4(3)
    internal_vec4(0) = x
    internal_vec4(1) = y
    internal_vec4(2) = z
    internal_vec4(3) = w
    glVec4%& = _OFFSET(internal_vec4())
END FUNCTION
