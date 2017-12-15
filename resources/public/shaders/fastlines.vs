struct LetterVars {
    int mIndex;
    vec3 mPos0;
    vec3 mPos1;
    vec4 mColour;
};

LetterVars getLetterVars(int vertexId, int a_spr, sampler2D samp) {

    int to_index[6] = int[](0, 1, 2, 3, 4, 5);

    LetterVars ret;

    int quad_vert_num = vertexId / 6;
    int quad_vert_id  = vertexId % 6;
    int mIndex        = to_index[quad_vert_id];

    ivec2 letter_texel = ivec2(quad_vert_num, a_spr );

    vec4 line = texelFetch(samp, letter_texel, 0);

    ret.mIndex  = mIndex;
    ret.mPos0   = vec3(line.xy,0.0);
    ret.mPos1   = vec3(line.zw,0.0);
    ret.mColour = vec4(1.0, 1.0, 1.0, 1.0);

    return ret;
}

void main() {
    mat4 mv = u_view * u_model;

    LetterVars letter = getLetterVars(gl_VertexID, a_spr, u_tex);

    vec3 pos0 = (mv * vec4(letter.mPos0, 1.0)).xyz;
    vec3 pos1 = (mv * vec4(letter.mPos1, 1.0)).xyz;

    LineVars line = makeLineVars3d( pos0, pos1,  u_radii, u_hardness, letter.mIndex);

    v_color          = letter.mColour;
    v_uv             = line.mUv ;
    v_radius         = line.mRadius;
    v_hardness       = line.mHardness;

    gl_Position   =  u_proj * vec4(line.mPos3, 1.0);
}

