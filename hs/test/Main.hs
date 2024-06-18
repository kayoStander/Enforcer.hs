module Main where

import Graphics.UI.GLUT
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile, withFile)

readFileContents :: String -> IO [String]
readFileContents filePath = do
  contents <-
    withFile
      filePath
      ReadMode
      ( \handle -> do
          contents <- hGetContents handle
          -- forces evaluation of a before return b
          -- good when trying to evaluate data from something that alredy has ben closed
          length contents `seq` return contents
      )
  return $ words contents

main :: IO ()
main = do
  -- GLUT Init
  (_, _) <- getArgsAndInitialize

  _ <- createWindow ""

  displayCallback $= display

  object <- readFileContents "resources/baseObj.obj"
  print object

  mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]

  color (Color3 1.0 0.0 0.0 :: Color3 GLfloat)

  let vert = do
        vertex (Vertex3 (-0.5) (-0.5) 0.0 :: Vertex3 GLfloat)
        vertex (Vertex3 0.5 (-0.5) 0.0 :: Vertex3 GLfloat)
        vertex (Vertex3 0.5 0.5 0.0 :: Vertex3 GLfloat)
        vertex (Vertex3 (-0.5) 0.5 0.0 :: Vertex3 GLfloat)

  renderPrimitive Quads vert
  -- renderPrimitive Triangles r

  flush
