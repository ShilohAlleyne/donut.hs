module Main where

import Data.List (transpose, zip4, intercalate)
import System.Console.ANSI

-- Lets pre-calculate as much as possible

screen :: Int
screen = 40

tbuffer :: [[Char]]
tbuffer = replicate screen (replicate screen ' ')

zbuffer :: [[Float]]
zbuffer = replicate screen (replicate screen 0.0)

illumination :: [Char]
illumination = ".,-~:;=!*#$@"

thetaSpacing, phiSpacing :: Float
thetaSpacing = 0.07
phiSpacing = 0.02

-- The rotation for 1600 frames
thetaAngles, phiAngles :: [Float]
thetaAngles = [1.07 + i * thetaSpacing | i <- [0..(40 * 40)]] -- A
phiAngles = [1.02 + i * phiSpacing | i <- [0..(40 * 40)]] -- B

-- Cos and Sin 
cosA, sinA :: [Float]
cosA = map cos thetaAngles
sinA = map sin thetaAngles

cosB, sinB :: [Float]
cosB = map cos phiAngles
sinB = map sin phiAngles 

-- Circles
r1, r2, k2, k1 :: Float
r1 = 1
r2 = 2
k2 = 5
k1 = fromIntegral screen * k2 * 3 / (8 * (r1 + r2))

(start, end) = (0, 2 * pi)

-- Generate the list of angles
phi, theta :: [Float]
phi = [start + fromIntegral i * phiSpacing | i <- [0..(315 - 1)]]
theta = [start + fromIntegral i * thetaSpacing | i <- [0..(90 -1)]]

-- Apply the cosine and sine functions to every element in the phi array
cosPhi, sinPhi, cosTheta, sinTheta :: [Float]
cosPhi = map cos phi
sinPhi = map sin phi
cosTheta = map cos theta
sinTheta = map sin theta

-- Generate list of circles
circleX, circleY :: [Float]
circleX = map ((+ r2) . (* r1)) cosTheta
circleY = map (* r1) sinTheta

frames :: [(Float, Float, Float, Float)]
frames = zip4 cosA sinA cosB sinB

--- Functions 

outer :: Num a => [a] -> [a] -> [[a]]
outer xs ys = [[x * y | y <- ys] | x <- xs]

setAt :: (Int, Int) -> a -> [[a]] -> [[a]]
setAt (i, j) val xs = [ if n == i then replace n val row else row | (n, row) <- zip [0..] xs ]
  where
    replace n val ys = [ if n == j then val else y | (n, y) <- zip [0..] ys ]

getRow :: Int -> [[a]] -> [a]
getRow row arr = arr !! row

getByCords :: (Int, Int) -> [[a]] -> a
getByCords (row, index) arr = getRow row arr !! index

mapByCords :: Int -> [[Int]] -> [[Int]] -> [[a]] -> [a]
mapByCords row arr1 arr2 arr3 = map (\(x,y) -> getByCords (x, y) arr3) cords
    where
        cords = zip (arr1 !! row) (arr2 !! row)

replace :: Int -> a -> [a] -> [a]
replace i x xs = let (ys, zs) = splitAt i xs in ys ++ [x] ++ tail zs

replaceByCords :: Int -> Int -> a -> [[a]] -> [[a]]
replaceByCords i j x arr = [if n == i then replace j x row else row | (n, row) <- zip [0..] arr]

updateBuffer' :: [[a]] -> Bool -> a -> Int -> Int -> [[a]]
updateBuffer' b m o x y = if m then replaceByCords x y o b else b

updateBuffer :: [[a]] -> [Bool] -> [a] -> [Int] -> [Int] -> [[a]]
updateBuffer b ms os xs ys = foldl update b (zip4 ms os xs ys)
  where
    update b (m, o, x, y) = updateBuffer' b m o x y

-- our function will map over a b, ca cs cb sb at once
renderFrame :: (Float, Float, Float, Float) -> [[Char]]
renderFrame (cA, sA, cB, sB) = recRender zbuffer tbuffer 89  
    where
        x1 = outer (zipWith (+) (map (* cB) cosPhi) (map ( * (sA * sB)) sinPhi)) circleX
        x2 = map ( * (cA * sB)) circleY
        x3 = transpose $ [zipWith (-) row x2 | row <- x1] -- 90, 315

        y1 = outer (zipWith (-) (map (* sB) cosPhi) (map ( * (sA * cB)) sinPhi)) circleX
        y2 = map ( * (cA * cB)) circleY
        y3 = transpose $ [zipWith (+) row y2 | row <- y1] -- 90, 315

        z = transpose $ [zipWith (+) (map ((+ k2) . (* cA)) row) (map (* sA) circleY) | row <- outer sinPhi circleX] -- 90, 315
        ooz = map (map (1/)) z -- 90, 315

        xp1 = [map (* k1) ([a * b | (a, b) <- zip row1 row2]) | (row1, row2) <- zip ooz x3]        
        xp2 = [[floor (x + (fromIntegral screen / 2)) | x <- row] | row <- xp1] -- 90, 315  

        yp1 = [map ( * k1) ([a * b | (a, b) <- zip row1 row2]) | (row1, row2) <- zip ooz y3]
        yp2 = [[floor ((fromIntegral screen / 2) - x) | x <- row] | row <- yp1] -- 90, 315

        l1 = [[x * sB | x <- row] | row <- outer cosPhi cosTheta]
        l2 = [[x * cA | x <- row] | row <- outer sinPhi cosTheta]
        l3 = [zipWith (-) x y | (x, y) <- zip l1 l2]
        l4 = map ( *sA) sinTheta
        l5 = [zipWith (-) row l4 | row <- l3] -- (L1) 315, 90

        l6 = map (* cA) sinTheta
        l7 = outer sinPhi (map (*sA) cosTheta)
        l8 = [zipWith (-) l6 row | row <- l7]
        l9 = [[x * cB | x <- row] | row <- l8] -- (L2) 315, 90

        l10 = [zipWith (+) x y | (x, y) <- zip l5 l9]
        l11 = [[x * 8 | x <- row] | row <- l10]
        l12 = transpose $ [[round x | x <- row] | row <- l11] -- (L) 90, 315

        maskL = [[x >= 0 | x <- row] | row <- l12] -- 90, 315
        chars = [[illumination !! x | x <- row] | row <- l12] -- 90, 315

        recRender :: [[Float]] -> [[Char]] -> Int -> [[Char]]
        recRender zbuffer tbuffer r = recRender' zbuffer tbuffer r
          where
            recRender' zbuffer tbuffer 0 = tbuffer
            recRender' zbuffer tbuffer r =
              let m = zipWith (&&) (getRow r maskL) (zipWith (>) (getRow r ooz) (mapByCords r xp2 yp2 zbuffer))
                  o = getRow r ooz
                  c = getRow r chars
                  x = getRow r xp2
                  y = getRow r yp2
                  newZbuffer = updateBuffer zbuffer m o x y
                  newTbuffer = updateBuffer tbuffer m c x y
              in recRender' newZbuffer newTbuffer (r-1)


pprint :: [[Char]] -> IO ()
pprint array = mapM_ putStrLn [unwords [char : "" | char <- row] | row <- array]


main :: IO ()
main = do
    print "Nah, I'd win"
    mapM_ (\frame -> do
        clearScreen
        setCursorPosition 0 0
        pprint $ renderFrame frame) frames

