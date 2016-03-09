import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GLU

import System.Exit
import Data.IORef

--タイマの間隔
timerInterval = 100

main = do
    --回転の角度を初期化
    rot <- newIORef 0.0
    arg <- newIORef 5.0
    cmp <- newIORef 10.0

    --GLUTの初期化
    (_progName, _args) <- getArgsAndInitialize
    initialDisplayMode $= [RGBAMode, DoubleBuffered]
    initialWindowSize $= Size 640 480
    
    --ウィンドウを作る
    createWindow "test3d"
    
    --表示に使うコールバック関数の指定
    displayCallback $= display rot arg cmp
    
    --ウィンドウのサイズが変更された時に呼ぶコールバック関数の指定
    reshapeCallback $= Just (reshape cmp)

    --キーボードのマウスのコールバック
    keyboardMouseCallback $= Just (keyboardProc arg cmp)
    
    --タイマを作る
    addTimerCallback timerInterval $ timerProc (display rot arg cmp)
    
    --GLUTのメインループに入る
    mainLoop

display rot arg cmp = do
    --回転させる
    w <- readIORef arg
    modifyIORef rot (+w)
    r <- readIORef rot
    p <- readIORef cmp

    --背景を黒にする
    clearColor $= Color4 0.0 0.0 0.0 0.0
    clear [ColorBuffer]
    
    --単位行列を読み込む
    loadIdentity

    lookAt (Vertex3 0.0 0.0 (10.0 + p)) (Vertex3 0.0 0.0 p) (Vector3 0.0 1.0 0.0)
   
    --表示
    preservingMatrix $ do
        rotate r (Vector3 0.0 1.0 0.0 :: Vector3 GLdouble)
        renderPrimitive Quads $ mapM_ vertex [
          Vertex3 1.0 1.0 0.0,
          Vertex3 (-1.0) (1.0) 0.0,
            Vertex3 (-1.0) (-1.0) 0.0,
            Vertex3 1.0 (-1.0) 0.0 :: Vertex3 GLfloat]
    
    --バッファの入れ替え
    swapBuffers

--タイマが呼ばれるたびにactを繰り返す
timerProc act = do
    act
    addTimerCallback timerInterval $ timerProc act
    
--ウィンドウのサイズが変更された時の処理
reshape cmp size@(Size w h)=do
    p <- readIORef cmp
    viewport $= (Position 0 0, size) --ウィンドウ全体を使う
    
    --ビューボリュームの設定
    matrixMode $= Projection
    loadIdentity
    perspective 60.0 (fromIntegral w / fromIntegral h) 0.001 50.0
    
    --少し後ろから撮影
    --lookAt (Vertex3 0.0 0.0 (-1.0)) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
    --lookAt (Vertex3 3.0 4.0 (-5.0)) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
    --lookAt (Vertex3 3.0 4.0 p) (Vertex3 0.0 0.0 0.0) (Vector3 0.0 1.0 0.0)
    matrixMode $= Modelview 0

keyboardProc arg cmp ch state _ _
    | ch     == Char 'q'    = exitWith ExitSuccess        --qが押されたら終了
    | ch     == Char 'z'    = modifyIORef arg (* 0)        --zが回転を止める
    | ch     == Char 'a'    = modifyIORef arg (+ 1)        --aが回転を加速する
    | ch     == Char 'b'    = modifyIORef cmp (+ 1)        --後進
    | ch     == Char 'f'    = modifyIORef cmp (+(-1))      --前進
    | ch     == Char 'u'    = modifyIORef cmp (+ 1)        --後進
    | ch     == Char 'd'    = modifyIORef cmp (+(-1))      --前進
    | state    == Down        = modifyIORef arg (*(-1))    --それ以外なら回転の方向を変える
    | otherwise            = return ()
