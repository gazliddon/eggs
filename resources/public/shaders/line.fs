
out vec4 out_col;

void main() {
	float d;

	vec2 uv = v_uv;

	float range = v_radius * 2.0f + 1.0;

	// Calculate distance from line
	if (uv.s < 0.0 ) {
	    // left side of capsule
		d  =  length( uv );
	} else if (uv.s > 1.0) {
	    // right side of capsule
		d  =  distance( uv, vec2(1.0,0.0) );
	} else {
	    // main body of capsule
	    
	    d =abs( uv.t ) ;
	}

	d /= v_radius;

	float blob = smoothstep( 1.001, v_hardness , d);

	vec4 mixed = mix( u_inner_color , u_outer_color, d);

    out_col = vec4( blob * v_color * mixed);
}

