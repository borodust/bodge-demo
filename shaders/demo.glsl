#version 330 core

#pragma bodge: import bodge/shadow

uniform mat4 view;

#ifdef BODGE_VERTEX_SHADER

in vec3 vPosition;
in vec3 vNormal;

uniform mat4 projection;
uniform mat4 model;

out vec3 normal;
out vec3 position;

void main () {
  mat4 viewModelMatrix = view * model;
  gl_Position = projection * viewModelMatrix * vec4(vPosition, 1.0);

  position = (model * vec4(vPosition, 1.0)).xyz;

  mat3 normalMatrix = inverse(transpose(mat3(viewModelMatrix)));
  normal = normalize(normalMatrix * vNormal);
}

#endif

#ifdef BODGE_FRAGMENT_SHADER

#pragma bodge: import bodge/phong

in vec3 normal;
in vec3 position;

uniform PhongMaterial material;
uniform PhongPointLight light;
uniform vec3 diffuseColor;
uniform vec3 emissionColor;
uniform samplerCube shadowMap;
uniform vec2 nearFar;

out vec4 fColor;

void main() {
  if (inShadow(position.xyz - light.position.xyz, shadowMap, nearFar.x, nearFar.y, 0.0001)) {
    fColor = vec4(emissionColor + light.ambient * diffuseColor, 1.0);
  } else {
    vec3 phongColor = calcPhongReflection(light, material, position, normal, diffuseColor, 1.0,
                                          mat3(view) * vec3(0.0, 0.0, -1.0));
    fColor = vec4(emissionColor + phongColor, 1.0);
  }
}

#endif
