#version 430 core

layout(location = 0) in vec4 vPosition;
layout(location = 1) in vec4 vertexColor;
//layout(location = 2) in vec2 uvCoords;

//out vec2 uv;
out vec4 fragmentColor;

void main()
{
  gl_Position = vPosition;

  fragmentColor = vertexColor;
  //uv = uvCoords;
}
