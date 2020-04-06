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

TYPE object_info
    start AS _UNSIGNED LONG
    length AS _UNSIGNED LONG
    init AS _BYTE
    mtl AS material_info
END TYPE

TYPE mesh_part_info
    start AS _UNSIGNED LONG
    length AS _UNSIGNED LONG
    init AS _BYTE
    mtl AS material_info
END TYPE

DECLARE LIBRARY 'camera control function
    SUB gluLookAt (BYVAL eyeX#, BYVAL eyeY#, BYVAL eyeZ#, BYVAL centerX#, BYVAL centerY#, BYVAL centerZ#, BYVAL upX#, BYVAL upY#, BYVAL upZ#)
END DECLARE


f$ = "young_tree.obj"
IF COMMAND$(1) <> "" THEN f$ = COMMAND$(1)

x = _INSTRREV(f$, "/") + _INSTRREV(f$, "\")
IF x = 0 THEN
    obj_file$ = f$
ELSE
    obj_file$ = RIGHT$(f$, LEN(f$) - x)
    path$ = LEFT$(f$, x)
END IF

DIM tag(4) AS STRING * 3, p(4) AS _UNSIGNED LONG
DIM SHARED v(4) AS _UNSIGNED LONG
REDIM vert(2) AS SINGLE, norm(2) AS SINGLE, texcoord(1) AS SINGLE
REDIM SHARED mesh(23) AS SINGLE '(3 vert + 2 tex coord + 3 norm )* 3 vert of triangle
REDIM SHARED materials(0) AS material_info, mesh_part(0) AS mesh_part_info
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
    mtl_file$ = path$ + _TRIM$(MID$(a$, x + 8, INSTR(x + 8, a$, LINE_FEED) - (x + 8)))
    IF NOT _FILEEXISTS(mtl_file$) THEN PRINT "ERROR : Could not load materials - " + mtl_file$: END
    x = 1
    materialPresent = 1
    mtl_index = -1 'so it start with 0 in the loop
    OPEN mtl_file$ FOR INPUT AS #2
    WHILE NOT EOF(2)
        LINE INPUT #2, b$
        IF LEFT$(b$, 1) <> "#" THEN
            IF LEFT$(b$, 6) = "newmtl" THEN
                mtl_index = mtl_index + 1
                REDIM _PRESERVE materials(mtl_index) AS material_info
                materials(mtl_index).id = _TRIM$(MID$(b$, 7, LEN(b$) - 6))
                materials(mtl_index).exits = 1
            ELSEIF LEFT$(b$, 2) = "Ka" THEN
                y1 = INSTR(4, b$, " ")
                y2 = INSTR(y1 + 1, b$, " ")
                materials(mtl_index).amb.x = VAL(MID$(b$, 4, y1 - 3))
                materials(mtl_index).amb.y = VAL(MID$(b$, y1, y2 - y1 + 1))
                materials(mtl_index).amb.z = VAL(RIGHT$(b$, LEN(b$) - y2 + 1))
            ELSEIF LEFT$(b$, 2) = "Kd" THEN
                y1 = INSTR(4, b$, " ")
                y2 = INSTR(y1 + 1, b$, " ")
                materials(mtl_index).diff.x = VAL(MID$(b$, 4, y1 - 3))
                materials(mtl_index).diff.y = VAL(MID$(b$, y1, y2 - y1 + 1))
                materials(mtl_index).diff.z = VAL(RIGHT$(b$, LEN(b$) - y2 + 1))
            ELSEIF LEFT$(b$, 2) = "Ks" THEN
                y1 = INSTR(4, b$, " ")
                y2 = INSTR(y1 + 1, b$, " ")
                materials(mtl_index).spec.x = VAL(MID$(b$, 4, y1 - 3))
                materials(mtl_index).spec.y = VAL(MID$(b$, y1, y2 - y1 + 1))
                materials(mtl_index).spec.z = VAL(RIGHT$(b$, LEN(b$) - y2 + 1))
            ELSEIF LEFT$(b$, 2) = "Ke" THEN
                y1 = INSTR(4, b$, " ")
                y2 = INSTR(y1 + 1, b$, " ")
                materials(mtl_index).emis.x = VAL(MID$(b$, 4, y1 - 3))
                materials(mtl_index).emis.y = VAL(MID$(b$, y1, y2 - y1 + 1))
                materials(mtl_index).emis.z = VAL(RIGHT$(b$, LEN(b$) - y2 + 1))
            ELSEIF LEFT$(b$, 2) = "Ns" THEN
                materials(mtl_index).shine = VAL(MID$(b$, 3, LEN(b$) - 2))
            ELSEIF LEFT$(b$, 2) = "d " THEN
                materials(mtl_index).trans = VAL(MID$(b$, 2, LEN(b$) - 1))
            ELSEIF LEFT$(b$, 6) = "map_Kd" THEN
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
_TITLE "Loading - " + f$
$CHECKING:OFF
p(0) = 1: p(1) = 1: p(2) = 1: p(3) = 1: p(4) = 1
o_first = INSTR(1, a$, tag(4)): o_second = INSTR(o_first + 1, a$, tag(4))
mtl_first = INSTR(1, a$, LINE_FEED + "usemtl"): mtl_second = INSTR(mtl_first + 1, a$, LINE_FEED + "usemtl")
mtl_id$ = _TRIM$(MID$(a$, mtl_first + 8, INSTR(mtl_first + 8, a$, LINE_FEED) - (mtl_first + 8)))
FOR j = 0 TO UBOUND(materials)
    IF RTRIM$(materials(j).id) = mtl_id$ THEN mtl_index = j: EXIT FOR
NEXT

DO
    x = INSTR(p(c), a$, tag(c))
    IF x > 0 THEN
        v(c) = v(c) + 1
        IF c = 0 THEN
            y1 = INSTR(x + 3, a$, " ")
            y2 = INSTR(y1 + 1, a$, " ")
            y3 = INSTR(y2 + 1, a$, LINE_FEED)
            vert(v_index) = VAL(MID$(a$, x + 3, y1 - (x + 3)))
            vert(v_index + 1) = VAL(MID$(a$, y1, y2 - y1))
            vert(v_index + 2) = VAL(MID$(a$, y2, y3 - y2))
            v_index = v_index + 3
            REDIM _PRESERVE vert(UBOUND(vert) + 3) AS SINGLE
        ELSEIF c = 1 THEN
            y1 = INSTR(x + 4, a$, " ")
            y2 = INSTR(y1 + 1, a$, LINE_FEED)
            texcoord(vt_index) = VAL(MID$(a$, x + 4, y1 - (x + 4)))
            texcoord(vt_index + 1) = -VAL(MID$(a$, y1, y2 - y1))
            vt_index = vt_index + 2
            REDIM _PRESERVE texcoord(UBOUND(texcoord) + 2) AS SINGLE
        ELSEIF c = 2 THEN
            y1 = INSTR(x + 4, a$, " ")
            y2 = INSTR(y1 + 1, a$, " ")
            y3 = INSTR(y2 + 1, a$, LINE_FEED)
            norm(vn_index) = VAL(MID$(a$, x + 4, y1 - (x + 4)))
            norm(vn_index + 1) = VAL(MID$(a$, y1, y2 - y1))
            norm(vn_index + 2) = VAL(MID$(a$, y2, y3 - y2))
            vn_index = vn_index + 3
            REDIM _PRESERVE norm(UBOUND(norm) + 3) AS SINGLE
        ELSEIF c = 3 THEN
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
            spc_1 = x + 2: spc_2 = INSTR(spc_1 + 1, a$, " ")
            y_max = INSTR(x + 1, a$, LINE_FEED)

            n = -1 'so start with 0 in while loop
            'get the each vertex info which are sperated by space
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
                    vt_r1 = (VAL(MID$(dat(0), z_ref(0, 0) + 1, z_ref(0, 1) - (z_ref(0, 0) + 1))) - 1) * 3
                    vn_r1 = (VAL(RIGHT$(dat(0), LEN(dat(0)) - z_ref(0, 1))) - 1) * 3
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
                        mesh(m_index + 3) = texcoord(vt_r3): mesh(m_index + 4) = texcoord(vt_r1 + 1)
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
$CHECKING:ON
PRINT TIMER - t; "s"
PRINT "No. of vertices : "; v(0)
PRINT "No. of tex coord : "; v(1)
PRINT "No. of normals : "; v(2)
PRINT "No. of objects : "; v(4)
PRINT "No. of faces : "; v(3)
PRINT "No. of materials : "; UBOUND(materials) + 1
SLEEP

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

    IF init = 0 THEN
        FOR i = 0 TO UBOUND(mesh_part)
            IF mesh_part(i).mtl.img_tex < -1 THEN mesh_part(i).mtl.gl_tex = feedGLTexture(mesh_part(i).mtl.img_tex)
        NEXT
        init = 1
        aspect = _WIDTH / _HEIGHT
    END IF
    IF tmp = 0 THEN t = TIMER
    tmp = tmp + 1
    IF TIMER - t >= 1 THEN dely = tmp: tmp = 0
    _glDisable _GL_MULTISAMPLE

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

    FOR i = 0 TO UBOUND(mesh_part)

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
'Then please report your bug at twitter, I'm @KingOfCoders
'or email me at ashishkushwahacb@gmail.com

'Have Fun!

'QB64 Rocks!! :)

_GLRENDER _BEHIND

DIM SHARED model_texture_file$, model_file$, use_material

use_material = 0
model_file$ = "models/pyramid.obj"
model_texture_file$ = ""
IF _COMMANDCOUNT = 1 OR _COMMANDCOUNT = 2 THEN model_file$ = COMMAND$(1)

'this variable can be set to "", if you don't have textures.
model_texture_file$ = ""
IF _COMMANDCOUNT = 1 THEN model_texture_file$ = ""
IF _COMMANDCOUNT = 2 THEN model_texture_file$ = COMMAND$(2)

_TITLE "OpenGL OBJ Loader - " + model_file$

'finding the model directory
FOR i = LEN(model_file$) TO 1 STEP -1
    ca$ = MID$(model_file$, i, 1)
    IF ca$ = "/" OR ca$ = "\" THEN path_to_model$ = LEFT$(model_file$, i): EXIT FOR
NEXT

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
    emission AS vec3
    tranparency AS DOUBLE
    shineness AS DOUBLE
    texture AS LONG
    gl_tex AS LONG
    mtl_name AS STRING * 128
    id AS _UNSIGNED LONG
END TYPE

TYPE model_definition
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
start_time# = TIMER
OPEN model_file$ FOR INPUT AS #f
WHILE NOT EOF(f)
    LINE INPUT #f, info$
    IF LEFT$(info$, 2) = "v " THEN v_count = v_count + 1
    IF LEFT$(info$, 2) = "vt" THEN vt_count = vt_count + 1
    IF LEFT$(info$, 2) = "vn" THEN vn_count = vn_count + 1
    IF LEFT$(info$, 2) = "f " THEN face_count = face_count + 1
WEND
CLOSE #f


DIM SHARED totalObjects
DIM vertices(v_count) AS vec3
DIM texture_coordinates(vt_count) AS vec3
DIM normals(vn_count) AS vec3
DIM SHARED model_faces(face_count) AS model_definition

v = 1
vt = 1
vn = 1
fc = 1

f = FREEFILE
OPEN model_file$ FOR INPUT AS #f
WHILE NOT EOF(f)
    LINE INPUT #f, model_data$
    SELECT CASE LEFT$(model_data$, 2)
        CASE "mt"
            IF Word$(model_data$, 1) = "mtllib" THEN
			print "Material found"
                material_file$ = Word$(model_data$, 2)
                IF INSTR(material_file$, ":") = 0 THEN material_file$ = path_to_model$ + material_file$
                '################ Materials #########################
                IF material_file$ <> "" AND _FILEEXISTS(material_file$) THEN
                
                    PRINT "Reading Material File : "; material_file$
                    
                    IF mtl = 0 THEN REDIM SHARED material(1) AS model_material: mtl = 1
                    
                    material_file_handle = FREEFILE
                    
                    OPEN material_file$ FOR INPUT AS #material_file_handle
                    
                    WHILE NOT EOF(material_file_handle)
                        LINE INPUT #material_file_handle, info$
                        IF LEFT$(info$, 6) = "newmtl" THEN
                            REDIM _PRESERVE SHARED material(mtl + 1) AS model_material
                            mtl = mtl + 1
                            material(mtl).mtl_name = Word$(info$, 2)
                            material(mtl).id = mtl
                        END IF
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
                            IF INSTR(temp_image_file$, ":") = 0 THEN temp_image_file$ = path_to_model$ + temp_image_file$
                            PRINT "Attempting to Load Image : - "; temp_image_file$
                            
                            IF _FILEEXISTS(temp_image_file$) THEN
                                PRINT "Image exists : "; temp_image_file$
                                temp_image_handle = _LOADIMAGE(temp_image_file$, 32)
                                
                                IF temp_image_handle < -1 THEN
                                    material(mtl).texture = temp_image_handle
                                    PRINT "Image loaded successfully : "; temp_image_file$
                                ELSE
                                    PRINT "Failed to load image : "; temp_image_file$
                                END IF
                                
                            ELSE
                                PRINT temp_image_file$; " : file doesn't exists"
                            END IF
                            
                        END IF
                        PRINT ".";
                    WEND
                    CLOSE #material_file_handle
                    use_material = -1
                END IF
            END IF
        CASE "o "
            o = o + 1
            current_object_name$ = Word$(model_data$, 2)
            PRINT "[Object:- "; current_object_name$; "]"
            totalObjects = totalObjects + 1
        CASE "us"
            IF Word$(model_data$, 1) = "usemtl" AND use_material THEN
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
            ' REDIM _PRESERVE vertices(v + 1) AS vec3
            v = v + 1
        CASE "vt"
            IF vt = 1 THEN PRINT: PRINT "Reading Texture Coordinates"
            texture_coordinates(vt).x = VAL(Word$(model_data$, 2))
            texture_coordinates(vt).y = VAL(Word$(model_data$, 3))
            ' REDIM _PRESERVE texture_coordinates(vt + 1) AS vec3
            vt = vt + 1
        CASE "vn"
            IF vn = 1 THEN PRINT: PRINT "Reading Normals"
            normals(vn).x = VAL(Word$(model_data$, 2))
            normals(vn).y = VAL(Word$(model_data$, 3))
            normals(vn).z = VAL(Word$(model_data$, 4))
            ' REDIM _PRESERVE normals(vn + 1) AS vec3
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
                
            IF use_material THEN
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
            
            ' REDIM _PRESERVE model_faces(fc + 1) AS model_definition
            fc = fc + 1
    END SELECT
    IF g MOD 200 = 0 THEN PRINT ".";
    g = g + 1
WEND
CLOSE #f
time_taken# = TIMER - start_time#

CLS

REDIM SHARED DONT_USE_GLH_Handle(1) AS DONT_USE_GLH_Handle_TYPE


DIM SHARED mouseX, mouseY, Z_Value, wired_frame, lights_allow
DIM SHARED eye AS vec3, cameraTarget AS vec3, FPS

eye.x = 0: eye.y = 2: eye.z = 10
cameraTarget.x = eye.x: cameraTarget.y = eye.y: cameraTarget.z = eye.z - 10

lights_allow = 1
wired_frame = -1

'clear all the keys
_KEYCLEAR

FrameRate = 60
ff## = timer
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
    PRINT "TIME TAKEN TO LOAD : "; time_taken#; "s"
	PRINT "Rendering @ ";FrameRate;" FPS"
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
	if ff## - timer>1 then
	    FrameRate = FPS
		FPS = 0
		ff## = timer
	end if
    _DISPLAY
LOOP


SUB _GL () STATIC

    IF NOT glAllow THEN EXIT SUB
    FPS = FPS + 1
    IF NOT glSetup THEN
        _glViewport 0, 0, _WIDTH, _HEIGHT
        aspect# = _WIDTH / _HEIGHT
        IF use_material THEN
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

    IF use_material OR LEN(model_texture_file$) THEN
        _glEnable _GL_TEXTURE_2D
    END IF

    IF lights_allow = 1 THEN
        _glEnable _GL_LIGHTING
        _glEnable _GL_LIGHT0

        _glLightfv _GL_LIGHT0, _GL_AMBIENT, glVec3(0.0, 0.0, 0.0)
        _glLightfv _GL_LIGHT0, _GL_DIFFUSE, glVec3(1.0, 1.0, 1.0)
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
        IF done_task1 = 0 THEN
            done_task1 = 1
            textured_model_buffer = _glGenLists(1)
            _glNewList textured_model_buffer, _GL_COMPILE
        END IF
        FOR o = 1 TO totalObjects
            IF done_task1 = 2 THEN
                _glPushMatrix 'push
                _glRotatef -mouseX * 1.5, 0, 1, 0 'rotate the world
                _glRotatef mouseY * 1.5, 1, 0, 0
                _glCallList textured_model_buffer
                _glPopMatrix
                EXIT FOR
            END IF
			surface_init = 0
            use_texture_in_opengl = 0
            '#################################################
            FOR i = 1 TO UBOUND(model_faces)
                IF model_faces(i).obj_id = o THEN
                    IF surface_init = 0 THEN
                        surface_init = 1

                        IF use_material THEN
                            IF model_faces(i).material.texture < -1 THEN

                                GLH_Select_Texture material(model_faces(i).material.id).gl_tex
                                _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
                                _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_LINEAR

                                use_texture_in_opengl = -1
                            END IF
                            
                            'TURN on blending according to our beautiful model needs.
                            IF model_faces(i).material.tranparency <> 1 THEN _glEnable _GL_BLEND ELSE _glDisable _GL_BLEND
                            
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
                    IF use_material THEN
                        IF last_used_material <> current_model_material THEN
                            last_used_material = current_model_material
                            _glEnd

                            IF model_faces(i).material.texture < -1 THEN

                                GLH_Select_Texture material(model_faces(i).material.id).gl_tex
                                _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MIN_FILTER, _GL_NEAREST
                                _glTexParameteri _GL_TEXTURE_2D, _GL_TEXTURE_MAG_FILTER, _GL_LINEAR

                                use_texture_in_opengl = -1
                            END IF
                            
                            IF model_faces(i).material.tranparency <> 1 THEN _glEnable _GL_BLEND ELSE _glDisable _GL_BLEND
                            
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
        NEXT
        IF done_task1 = 1 THEN
            done_task1 = 2
            _glEndList
        END IF
    ELSE
        '######## Code optimize for faster rendering ######
        IF done_task2 = 0 THEN
            done_task2 = 1
            wire_frame_model_buffer = _glGenLists(1)
            _glNewList wire_frame_model_buffer, _GL_COMPILE
        END IF

        '####################################################
        _glColor3f 1, 1, 1
        FOR i = 1 TO UBOUND(model_faces)
            IF done_task2 = 2 THEN
                _glPushMatrix 'push
                _glRotatef -mouseX * 1.5, 0, 1, 0 'rotate the world
                _glRotatef mouseY * 1.5, 1, 0, 0
                _glCallList wire_frame_model_buffer
                _glPopMatrix
                EXIT FOR
            END IF
            _glBegin _GL_LINE_LOOP
            _glNormal3f model_faces(i).v1.vn.x, model_faces(i).v1.vn.y, model_faces(i).v1.vn.z
            _glVertex3f model_faces(i).v1.v.x, model_faces(i).v1.v.y, model_faces(i).v1.v.z

            _glNormal3f model_faces(i).v2.vn.x, model_faces(i).v2.vn.y, model_faces(i).v2.vn.z
            _glVertex3f model_faces(i).v2.v.x, model_faces(i).v2.v.y, model_faces(i).v2.v.z

            _glNormal3f model_faces(i).v3.vn.x, model_faces(i).v3.vn.y, model_faces(i).v3.vn.z
            _glVertex3f model_faces(i).v3.v.x, model_faces(i).v3.v.y, model_faces(i).v3.v.z
            _glEnd
        NEXT
        IF done_task2 = 1 THEN
            done_task2 = 2
            _glEndList
        END IF
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

$CHECKING:OFF
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
    REDIM _PRESERVE DONT_USE_GLH_Handle(UBOUND(DONT_USE_GLH_HANDLE)+1) AS DONT_USE_GLH_Handle_TYPE
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
