
// Common glsl used in all shaders

vec4 getBlobLow(vec2 _uv, float _whiteDist, float _blackDist) {
	vec4 scale = vec4(1,1,1,1);
    vec2 uv = _uv - 0.5;
    float d = length(uv);
    d = smoothstep( _blackDist, _whiteDist, d);
    vec4 v = d * scale;
    return v;
}

vec4 getBlob(vec2 _uv) {
    return getBlobLow(_uv, 0.1, 0.5);
}

mat2 rotate2d(float ang) {
    float c = cos(ang);
    float s = sin(ang);
    return mat2( c,-s,s,c);
}

mat4 rotationM(vec3 axis, float angle) {

    axis     = normalize(axis);

    float s  = sin( angle );
    float c  = cos( angle );
    float oc = 1.0 - c;
    float x  = axis.x;
    float y  = axis.y;
    float z  = axis.z;

    return mat4(
            oc * x * x + c,     oc * x * y - z * s, oc * z * x + y * s, 0.0,
            oc * x * y + z * s, oc * y * y + c,     oc * y * z - x * s, 0.0,
            oc * z * x - y * s, oc * y * z + x * s, oc * z * z + c,     0.0,
            0.0,                0.0,                0.0,                1.0 );
}
mat4 scaleM(vec3 scale) {
    return mat4(
            scale.x,   0.0,        0.0,       0.0,
            0.0,       scale.y,    0.0,       0.0,
            0.0,       0.0,        scale.z,   0.0,
            0.0,       0.0,        0.0,       1.0);
}

mat4 translationM(vec3 tr) {
    return mat4(
            1.0,  0.0,   0.0,  0.0,
            0.0,  1.0,   0.0,  0.0,
            0.0,  0.0,   1.0,  0.0,
            tr.x, tr.y,  tr.z, 1.0);
}

struct LineVars {
    vec2 mUv;
    vec2 mPos;
    float mRadius;
    float mHardness;
};

LineVars makeLineVars3d(vec2 _p0, vec2 _p1, vec2 _radii, vec2 _hardness, int _index ) {
    LineVars ret;

    vec2 dir = normalize(_p1 - _p0);
    float ang = atan(dir.y, dir.x);
    mat2 m = rotate2d(-ang );

    float startRadius      = _radii.x;
    float endRadius        = _radii.y;
    float invLen           =  1.0 / distance(_p0, _p1);

    float startSmallRadius = startRadius * invLen;
    float endSmallRadius   = endRadius   * invLen;

	if ( _index == 0 ) {

		ret.mUv           = vec2( 0.0 - startSmallRadius,  -startSmallRadius);
		ret.mPos          = (m * vec2(-1,-1) * startRadius) + _p0;
		ret.mRadius       = startSmallRadius;
		ret.mHardness     = _hardness.x;

	} else if (_index == 1) {

		ret.mUv           = vec2( 1.0 + endSmallRadius,    -endSmallRadius);
		ret.mPos          = (m * vec2(1,-1) * endRadius  ) + _p1,
		ret.mRadius       = endSmallRadius;
		ret.mHardness     = _hardness.y;

	} else if (_index == 2) {
		ret.mUv           = vec2( 1.0 + endSmallRadius,     endSmallRadius);
		ret.mPos          = m * (vec2(  1,  1) * endRadius  ) + _p1;
		ret.mRadius       = endSmallRadius;
		ret.mHardness     = _hardness.y;

	} else {
		ret.mUv           = vec2( 0.0 - startSmallRadius,   startSmallRadius);
		ret.mPos          = m * (vec2( -1,  1) * startRadius) + _p0;
		ret.mRadius       = startSmallRadius;
		ret.mHardness     = _hardness.x;
	}

/*

    vec2 uv[4]= vec2[4] (
    		vec2( 0.0 - startSmallRadius,  -startSmallRadius),  // TL
    		vec2( 1.0 + endSmallRadius,    -endSmallRadius),  // TR
    		vec2( 1.0 + endSmallRadius,     endSmallRadius),  // BR
    		vec2( 0.0 - startSmallRadius,   startSmallRadius)   // BL
	);

    vec2 mpos[4] = vec2[4] (
            vec2( -1, -1),
            vec2(  1, -1),
            vec2(  1,  1),
            vec2( -1,  1));

    vec2 pos[4] = vec2[4] (
            m * (mpos[0] * startRadius) + _p0,
            m * (mpos[1] * endRadius  ) + _p1,
            m * (mpos[2] * endRadius  ) + _p1,
            m * (mpos[3] * startRadius) + _p0
            );

	float radii[4] = float[4] (
	        startSmallRadius, 
	        endSmallRadius,
	        endSmallRadius,
	        startSmallRadius
            );

    float hardness[4] = float[4](
            _hardness.x,
            _hardness.y,
            _hardness.y, 
            _hardness.x
            );

    ret.mUv           = uv[index];
    ret.mPos          = pos[index];
    ret.mRadius       = radii[index];
    ret.mHardness     = hardness[index];
*/

    return ret;
}


LineVars makeLineVars(vec2 _p0, vec2 _p1, vec2 _radii, vec2 _hardness, int _index )
{
    LineVars ret;

    vec2 dir = normalize(_p1 - _p0);
    float ang = atan(dir.y, dir.x);
    mat2 m = rotate2d(-ang );

    float startRadius      = _radii.x;
    float endRadius        = _radii.y;
    float invLen           =  1.0 / distance(_p0, _p1);

    float startSmallRadius = startRadius * invLen;
    float endSmallRadius   = endRadius   * invLen;

	if ( _index == 0 ) {

		ret.mUv           = vec2( 0.0 - startSmallRadius,  -startSmallRadius);
		ret.mPos          = (m * vec2(-1,-1) * startRadius) + _p0;
		ret.mRadius       = startSmallRadius;
		ret.mHardness     = _hardness.x;

	} else if (_index == 1) {

		ret.mUv           = vec2( 1.0 + endSmallRadius,    -endSmallRadius);
		ret.mPos          = (m * vec2(1,-1) * endRadius  ) + _p1,
		ret.mRadius       = endSmallRadius;
		ret.mHardness     = _hardness.y;

	} else if (_index == 3) {
		ret.mUv           = vec2( 1.0 + endSmallRadius,     endSmallRadius);
		ret.mPos          = m * (vec2(  1,  1) * endRadius  ) + _p1;
		ret.mRadius       = endSmallRadius;
		ret.mHardness     = _hardness.y;

	} else {
		ret.mUv           = vec2( 0.0 - startSmallRadius,   startSmallRadius);
		ret.mPos          = m * (vec2( -1,  1) * startRadius) + _p0;
		ret.mRadius       = startSmallRadius;
		ret.mHardness     = _hardness.x;
	}

/*

    vec2 uv[4]= vec2[4] (
    		vec2( 0.0 - startSmallRadius,  -startSmallRadius),  // TL
    		vec2( 1.0 + endSmallRadius,    -endSmallRadius),  // TR
    		vec2( 1.0 + endSmallRadius,     endSmallRadius),  // BR
    		vec2( 0.0 - startSmallRadius,   startSmallRadius)   // BL
	);

    vec2 mpos[4] = vec2[4] (
            vec2( -1, -1),
            vec2(  1, -1),
            vec2(  1,  1),
            vec2( -1,  1));

    vec2 pos[4] = vec2[4] (
            m * (mpos[0] * startRadius) + _p0,
            m * (mpos[1] * endRadius  ) + _p1,
            m * (mpos[2] * endRadius  ) + _p1,
            m * (mpos[3] * startRadius) + _p0
            );

	float radii[4] = float[4] (
	        startSmallRadius, 
	        endSmallRadius,
	        endSmallRadius,
	        startSmallRadius
            );

    float hardness[4] = float[4](
            _hardness.x,
            _hardness.y,
            _hardness.y, 
            _hardness.x
            );

    ret.mUv           = uv[index];
    ret.mPos          = pos[index];
    ret.mRadius       = radii[index];
    ret.mHardness     = hardness[index];
*/

    return ret;
}


