void main() {

    mat4 mvp = u_proj * u_view * u_model;
    mat4 mv = u_view * u_model;

    LineVars vars;

    vec3 pos0 = (mv * vec4(a_position0, 1.0)).xyz;
    vec3 pos1 = (mv * vec4(a_position1, 1.0)).xyz;

    vars = makeLineVars3d( pos0, pos1, a_radii * u_radii, u_hardness, a_index);

    vec4 cols[4] = vec4[] (
    		a_color0,
    		a_color1,
    		a_color1,
    		a_color0
    		);

    v_color          = cols[a_index] ;
    v_uv             = vars.mUv ;
    v_radius         = vars.mRadius;
    v_hardness       = vars.mHardness;

    gl_Position   =  u_proj * vec4(vars.mPos3, 1.0);
}

