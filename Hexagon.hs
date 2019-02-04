module Hexagon where

import Graphics.Gloss

w :: Float
w = sqrt 3 / 2

size :: Float
size = 25

type Axial = (Float,Float)
type Cube  = (Float,Float,Float)

axialRound :: Axial -> Axial
axialRound = cubeToAxial . cubeRound . axialToCube

cubeRound :: Cube -> Cube
cubeRound (x,y,z) | xd > yd && xd > zd = (-ry-rz,ry,rz)
                  | yd > zd            = (rx,-rx-rz,rz)
                  | otherwise          = (rx,ry,-rx-ry)
                  where
                    rx = fromIntegral (round x)
                    ry = fromIntegral (round y)
                    rz = fromIntegral (round z)

                    xd = abs (rx - x)
                    yd = abs (ry - y)
                    zd = abs (rz - z)

axialToCube :: Axial -> Cube
axialToCube (x,y) = (x,y,-x-y)

cubeToAxial :: Cube -> Axial
cubeToAxial (x,y,_) = (x,y)

pixelToAxial :: Axial -> Axial
pixelToAxial (x,y) = axialRound ((x * sqrt 3 / 3 - y / 3) / size, y * 2/3 / size)

axialToPixel :: Axial -> Point
axialToPixel (x,y) = ((2*x+y)*size*w,size*1.5*y)

cubeToPixel :: Cube -> Point
cubeToPixel = axialToPixel . cubeToAxial

cubeDistance :: Cube -> Cube -> Float
cubeDistance (x1,y1,z1) (x2,y2,z2) = (abs (x1-x2) + abs (y1-y2) + abs (z1-z2)) / 2

axialDistance :: Axial -> Axial -> Float
axialDistance x y = cubeDistance (axialToCube x) (axialToCube y)

cubeAdjacents :: Cube -> [Cube]
cubeAdjacents = cubeRing 1

axialAdjacents :: Axial -> [Axial]
axialAdjacents = axialRing 1

cubeRotateR :: Cube -> Cube -> Cube
cubeRotateR (cx,cy,cz) (x,y,z) = (-z+cz+cx,-x+cx+cy,-y+cy+cz)

cubeRotateL :: Cube -> Cube -> Cube
cubeRotateL (cx,cy,cz) (x,y,z) = (-y+cy+cx,-z+cz+cy,-x+cx+cz)

cubeRing :: Int -> Cube -> [Cube]
cubeRing 0 c       = [c]
cubeRing r (x,y,z) = concat $ take 6 $ iterate (map (cubeRotateR (x,y,z))) $ take r (iterate (\(x',y',z') -> (x',y'+1,z'-1)) (x+fromIntegral r,y-fromIntegral r,z))

cubeSpiral :: Int -> Cube -> [Cube]
cubeSpiral r c = concatMap (flip cubeRing c) [0..r]

axialRing :: Int -> Axial -> [Axial]
axialRing r = map cubeToAxial . cubeRing r . axialToCube

axialSpiral :: Int -> Axial -> [Axial]
axialSpiral r = map cubeToAxial . cubeSpiral r . axialToCube

drawHex :: (a -> Point) -> ([Point] -> Picture) -> a -> Picture
drawHex f g p = let (x,y) = f p in g [(x-size*w,y-size*0.5),(x,y-size),(x+size*w,y-size*0.5),(x+size*w,y+size*0.5),(x,y+size),(x-size*w,y+size*0.5)]
