#version 330 core

#pragma bodge: use amalgam cl-bodge.test.demo::test-amalgam as uniform list

#ifdef BODGE_VERTEX_SHADER

void main () {
  gl_Position = vec4(0.0, 0.0, 0.0, 1.0);
  gl_PointSize = 200.0;
}

#endif

#ifdef BODGE_FRAGMENT_SHADER

out vec4 fColor;

void main() {
  fColor = color;
}

#endif
