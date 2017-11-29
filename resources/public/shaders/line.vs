void main() {

    mat4 vp = u_proj * u_view;

    LineVars vars;

    vec2 pos0 = (u_model * vec4(a_position0, 1.0)).xy;
    vec2 pos1 = (u_model * vec4(a_position1, 1.0)).xy;

    vars = makeLineVars( pos0, pos1, a_radii * u_radii, u_hardness, a_index);

    vec4 cols[4] = vec4[] (
    		a_color0,
    		a_color1,
    		a_color0,
    		a_color1);

    v_color          = cols[a_index] ;
    v_uv             = vars.mUv ;
    v_radius         = vars.mRadius;
    v_hardness       = vars.mHardness;

    gl_Position   =  vp * vec4(vars.mPos, 0.0, 1.0);
}

