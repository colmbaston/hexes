module Main where

import           Hexagon
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import           System.Random
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do w <- getRandomWorld
          playIO
            (InWindow "Hex" (1000, 1000) (10, 10))
            black
            60
            w
            (return . drawWorld)
            (\x -> return . handler x)
            (\t w -> return $ w { time = time w + t })

getRandomWorld :: IO World
getRandomWorld = do ts <- M.fromList <$> mapM (\p -> (if p == (0,0) then pure Land else randomTile) >>= \t -> pure (p,t)) (axialSpiral 100 (0,0))
                    pure (World (0,0) 0 ts (visibleFrom (0,0) ts) S.empty)

data Tile = Land | Water deriving (Eq,Ord)

randomTile :: IO Tile
randomTile = do r <- randomRIO (0,3) :: IO Int
                pure (if r == 0 then Water else Land)

tileColor :: Tile -> Color
tileColor Land  = dark green
tileColor Water = blue

animation :: Float -> (Float,Float) -> Tile -> Picture
animation _ _ Land  = blank
animation t p Water = uncurry translate (axialToPixel p) $ (if odd (round (snd p)) then id else scale (-1) 1) (drawFun sin 20 t (size*2*w) (size/4))

drawVisibleTile :: Float -> (Float,Float) -> Tile -> Picture
drawVisibleTile t p b = pictures [color (tileColor b) (axialHexagon polygon p), color white $ animation t p b, axialHexagon lineLoop p]

drawVisitedTile :: (Float,Float) -> Tile -> Picture
drawVisitedTile p t = pictures [drawVisibleTile 0 p t, color (withAlpha 0.75 black) (axialHexagon polygon p)]

axialHexagon :: ([Point] -> Picture) -> (Float,Float) -> Picture
axialHexagon f c = case axialToPixel c of
                     (x,y) -> f [(x-size*w,y-size*0.5),(x,y-size),(x+size*w,y-size*0.5),(x+size*w,y+size*0.5),(x,y+size),(x-size*w,y+size*0.5)]

handler :: Event -> World -> World
handler (EventKey (MouseButton LeftButton) Down (Modifiers Up Up Up) p) w = let p' = pixelToAxial p in
                                                                              if p' `notElem` S.fromList (axialAdjacents (selected w)) || M.lookup p' (tiles w) /= Just Land
                                                                                 then w
                                                                                 else let new = visibleFrom p' (tiles w) in
                                                                                        w { selected = p',
                                                                                            visible  = new,
                                                                                            visited  = visited w `S.union` visible w `S.difference` new }
handler _ w = w

data World = World { selected :: (Float,Float), time :: Float, tiles :: Map (Float,Float) Tile, visible :: Set (Float,Float), visited :: Set (Float,Float) }

drawWorld :: World -> Picture
drawWorld w = pictures (map (\p -> drawVisibleTile (time w) p (fromJust $ M.lookup p (tiles w))) (S.toList (visible w))
                    ++  map (\p -> drawVisitedTile          p (fromJust $ M.lookup p (tiles w))) (S.toList (visited w))
                    ++ [(color red . uncurry translate (axialToPixel (selected w))) (thickCircle 0 (size / 2))])

drawFun :: (Float -> Float) -> Int -> Float -> Float -> Float -> Picture
drawFun f n t x y = line (take (n+1) $ zip xs vs)
  where
    xs :: [Float]
    xs = iterate (+(x / fromIntegral n)) (negate (x/2))

    vs :: [Float]
    vs = map ((*(y/2)) . f) (iterate (+((2*pi) / fromIntegral n)) t)

visibleFrom :: (Float,Float) -> Map (Float,Float) Tile -> Set (Float,Float)
visibleFrom p = M.keysSet . M.filterWithKey (\p' _ -> p' `elem` axialSpiral 2 p)
