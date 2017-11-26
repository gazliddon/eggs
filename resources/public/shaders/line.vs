void main() {
    LineVars vars;

    float l = 1.0;

    int vid = gl_VertexID & 3;

    vec2 pos0 = (u_model * vec4(a_position0, 0.0, 1.0)).xy;
    vec2 pos1 = (u_model * vec4(a_position1, 0.0, 1.0)).xy;

    vec2 v = pos1 - pos0;
    pos0 = pos0 + ((1.0-l) / 2.0) * v;
    pos1 = pos0 + v * l;

    vars = makeLineVars( pos0, pos1, a_radii * u_radii, u_hardness, vid);

    vec4 cols[4] = vec4[] (
    		a_color0,
    		a_color1,
    		a_color0,
    		a_color1);

    v_color          = cols[a_index] ;
    v_uv             = vars.mUv ;
    v_radius         = vars.mRadius;
    v_hardness       = vars.mHardness;

    gl_Position   =  u_vp * vec4(vars.mPos, 0.0, 1.0);
}

