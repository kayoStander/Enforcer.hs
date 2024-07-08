#version 430 core

layout(location = 0) in vec4 vPosition;
layout(location = 2) in vec2 uvCoords;

out vec2 uv;

void main()
{
   gl_Position = vPosition;

  uv = uvCoords;
}
