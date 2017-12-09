void main() {

    int vert_num = gl_VertexID / 6;
    int v_id = gl_VertexID % 6;
    int to_index[6] = int[](0 1 2 3 4 5);
    int a_index = to_index[v_id]

    vec4 line = texelFetch(tex_id, ivec2(vert_num, a_spr), 0);

    vec3 a_position0 = vec3(line.xy, 0);
    vec3 a_position1 = vec3(line.zw, 0);

    mat4 mvp = u_proj * u_view * u_model;
    mat4 mv = u_view * u_model;

    LineVars vars;

    vec3 pos0 = (mv * vec4(a_position0, 1.0)).xyz;
    vec3 pos1 = (mv * vec4(a_position1, 1.0)).xyz;

    vars = makeLineVars3d( pos0, pos1,  u_radii, u_hardness, a_index);

    vec4 cols[4] = vec4[] (
        vec4(1.0),
        vec4(1.0),
        vec4(1.0),
        vec4(1.0));

    v_color          = vec4(1.0);
    v_uv             = vars.mUv ;
    v_radius         = vars.mRadius;
    v_hardness       = vars.mHardness;

    gl_Position   =  u_proj * vec4(vars.mPos3, 1.0);
}

