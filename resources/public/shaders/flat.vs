void main() {
    v_color = a_color;
    gl_Position =  u_proj * u_view * u_model * vec4(a_pos, 1.0);
}

