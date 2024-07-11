#version 430 core

//in vec2 uv;
//uniform sampler2D tex;

in vec4 fragmentColor;

out vec4 fColor;

void main()
{
  fColor = fragmentColor;
  //fColor = vec4(texture(tex,uv).rgb, 1.0)*fragmentColor;
}
