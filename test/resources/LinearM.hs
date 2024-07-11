{-# LANGUAGE OverloadedStrings #-}

module LinearM where

-- import Control.Applicative
import qualified Graphics.GLUtil as U
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.UI.GLUT as GLUT
import qualified Linear as L
import System.FilePath ((</>))

main :: IO ()
main = do
  win <- createWindow "Window" (512, 512)
  prog <- initResources
  mainLoop prog win
  closeWindow win

createWindow :: String -> (Int, Int) -> IO GLFW.Window
createWindow title (sizeX, sizeY) = do
  _ <- GLFW.init
  GLFW.defaultWindowHints
  GLUT.initialDisplayMode $= [GLUT.RGBAMode]
  Just win <- GLFW.createWindow sizeX sizeY title Nothing Nothing
  GLFW.makeContextCurrent (Just win)
  return win

mainLoop :: Resources -> GLFW.Window -> IO ()
mainLoop prog win = do
  draw prog win
  mainLoop prog win

closeWindow :: GLFW.Window -> IO ()
closeWindow win = do
  GLFW.destroyWindow win
  GLFW.terminate

initResources :: IO Resources
initResources = do
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

  let v = "test/shaders/shader.vert.glsl"
      f = "test/shaders/shader.frag.glsl"
  Resources
    <$> U.simpleShaderProgram v f
    <*> U.makeBuffer GL.ArrayBuffer vertices
    <*> U.makeBuffer GL.ArrayBuffer colors

draw :: Resources -> GLFW.Window -> IO ()
draw r win = do
  GL.clearColor $= GL.Color4 1 1 1 1
  GL.clear [GL.ColorBuffer]
  (width, height) <- GLFW.getFramebufferSize win
  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))

  t <- maybe 0 id <$> GLFW.getTime
  GL.currentProgram $= (Just . U.program . triProgram $ r)
  U.enableAttrib (triProgram r) "coord2d"
  U.enableAttrib (triProgram r) "v_color"

  GL.bindBuffer GL.ArrayBuffer $= Just (vertBuffer r)
  U.setAttrib
    (triProgram r)
    "coord2d"
    GL.ToFloat
    $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
  GL.bindBuffer GL.ArrayBuffer $= Just (colorBuffer r)
  U.setAttrib
    (triProgram r)
    "v_color"
    GL.ToFloat
    $ GL.VertexArrayDescriptor 3 GL.Float 0 U.offset0
  U.setUniform (triProgram r) "fade" (fade t)
  GL.drawArrays GL.Triangles 0 3

  GL.vertexAttribArray (U.getAttrib (triProgram r) "coord2d") $= GL.Disabled
  GL.vertexAttribArray (U.getAttrib (triProgram r) "v_color") $= GL.Disabled

data Resources = Resources {triProgram :: U.ShaderProgram, vertBuffer :: GL.BufferObject, colorBuffer :: GL.BufferObject}

fade :: Double -> GL.Index1 GL.GLfloat
fade t = GL.Index1 . realToFrac $ sin (t * 2 * pi / 5) / 2 + 0.5

shaderPath :: FilePath
shaderPath = "wikibook " </> "idk"

vertices :: [L.V2 Float]
vertices =
  [ L.V2 0.0 0.8,
    L.V2 (-0.8) (-0.8),
    L.V2 0.8 (-0.8)
  ]

colors :: [L.V3 Float]
colors = [L.V3 1 1 0, L.V3 0 0 1, L.V3 1 0 0]
