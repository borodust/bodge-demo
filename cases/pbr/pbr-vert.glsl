#version 330 core

#define HAS_NORMALS 1
#define HAS_TANGENTS 1
#define HAS_UV 1

//
// Adapted from https://github.com/KhronosGroup/glTF-WebGL-PBR/blob/master/shaders/pbr-frag.glsl
//

/*
The MIT License

Copyright (c) 2016-2017 Mohamad Moneimne and Contributors

Permission is hereby granted, free of charge, to any person obtaining a copy of this software
and associated documentation files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

in vec3 a_Position;
#ifdef HAS_NORMALS
in vec3 a_Normal;
#endif
#ifdef HAS_TANGENTS
in vec3 a_Tangent;
#endif
#ifdef HAS_UV
in vec2 a_UV;
#endif

uniform mat4 u_MVPMatrix;
uniform mat4 u_ModelMatrix;
uniform mat3 u_NormalMatrix;

out vec3 v_Position;
out vec2 v_UV;

#ifdef HAS_NORMALS
#ifdef HAS_TANGENTS
out mat3 v_TBN;
#else
out vec3 v_Normal;
#endif
#endif


void main()
{
  vec4 pos = u_ModelMatrix * vec4(a_Position, 1.0);
  v_Position = vec3(pos.xyz) / pos.w;

  #ifdef HAS_NORMALS
  #ifdef HAS_TANGENTS
  vec3 normalW = normalize(vec3(u_NormalMatrix * a_Normal));
  vec3 tangentW = normalize(vec3(u_ModelMatrix * vec4(a_Tangent, 0.0)));
  vec3 bitangentW = cross(normalW, tangentW);
  v_TBN = mat3(tangentW, bitangentW, normalW);
  #else // HAS_TANGENTS != 1
  v_Normal = normalize(vec3(u_ModelMatrix * vec4(a_Normal, 0.0)));
  #endif
  #endif

  #ifdef HAS_UV
  v_UV = a_UV;
  #else
  v_UV = vec2(0.,0.);
  #endif

  gl_Position = u_MVPMatrix * vec4(a_Position, 1.0); // needs w for proper perspective correction
}
