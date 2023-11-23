{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import Graphics.Gloss.Interface.Pure.Display
--import Data.Vector as V
--import qualified Data.Map.Lazy as M

--import Control.Monad
import Debug.Trace

import System.Random
import Numeric.Extra
--import Data.Free
import Data.Maybe

--import Control.Lens
--import Data.SBV

{-
Вообще программа простая, если игнорировать синтаксис и мой код. 
у drowFXTBSBP_Clos_SimpleВ параметре "количество точек" лучше ставить 1 или 0, по сколькуфактическое количество точек больше (при 0 = 2. 1 = 4. 2 = 6),
Пака что все точки близко друг другу, так что уже при 1 и 2 всё скучно.

В параметре "глубина" оптимально 4 или 8, иначе расстояние
меду точками может стремиться к 0.

Я вышлю токо исходники. Если будут интересно что делают остальные функции 
можете по вызывать их из интерпретатора  ghci (:l Main ).

Для корректной работы нужны доп пакеты, а именно: gloss, extra.устанавливаются через 
cabal: "cabal update
cabal install gloss"stack: аналогично
-}

data LLine a = LLine [a] a [a]
			| ZeroLine 

unt = undefined

disp = InWindow "LOL" (500,500) (0,0)

main = do
	--                          количество многочленов     параметры генерации (eps, глубина, и количество точек)
	pic <- fmap Pictures $ sequence $ (take 5) $ repeat $ drowFXTBSBP_Clos_Simple 0.1 4 0 -- не берите большие числа (+- около текущих нормально) 
--	display disp black ((Color white . fancePic) pic) -- заборчик
	display disp black ((Color white . picTo4Line . picLineP2 10 ) pic) -- типо самоподобие
-- RandomBinarSqrtBinarPoint

fancePic :: Picture -> Picture
fancePic p = forFun (\pic -> Pictures [pic,Translate (2) (6) pic] ) 3 $ (\pic -> Pictures [pic,Scale (1) (-1) pic] ) $ forFun fun 5 $ Pictures [p,p']
	where
		p' = Translate (4) (0) $ Scale (-1) (1) p
		fun = (\pic-> Pictures [pic,Translate (4) (0) pic])
		forFun :: (Picture -> Picture) -> Int -> Picture -> Picture
		forFun f 0 p = p
		forFun f i p = forFun f (i-1) (f p)

picLineP2 :: Int -> Picture -> Picture
picLineP2 n p = Pictures $ take n $ linlp p
	where
		linlp :: Picture -> [Picture]
		linlp pi = pi:(linlp (Scale 2 2 pi) )

picTo4Line :: Picture -> Picture
picTo4Line pic = Pictures [lineF,oyLi,le,pic, Scale (-1) (1) oyLi]
	where
		le = Scale (-1) (1) pic
		lineF = Scale (1) (-1) $ Pictures [le,pic]
		oyLi  = Rotate 90 lineF

type IntersecBSB = (Int -> Int -> (Float,Float))

bsbRR :: IntersecBSB
bsbRR n' k' = (x,y)
	where
		n = intToFloat n'
		k = intToFloat k'
		z = (2**(2*n) + 2**(2*k))
		x = (2**(2*n + k))/z
		y = (2**(n + 2*k))/z

bsbRSR :: IntersecBSB
bsbRSR n' k' = (x,y)
	where
		n = intToFloat n'
		k = intToFloat k'
		z = (2**(2*n) + 2**k)
		x = (2**(2*n + (k/2)))/z
		y = (2**(n + k))/z

bsbRSL :: IntersecBSB
bsbRSL n' k' | (2 * n) >= k = (x,y)
	where
		n = intToFloat n'
		k = intToFloat k'
		x = 2**(k/2)
		y = (2**n) - (sqrt (2 ** (2 * n) - 2 ** k))
bsbRSL _ _ = (0,0)

{-}
type AVector = (Float,Float)
data Edge = Edge
	{ _vector :: AVector
	, _vertex1 :: (Float,Float)
	, _vertex2 :: (Float,Float)
	, _rvector :: Flaot
	}

makeLenses 'Edge

data AForm = AForm [Edge]
		   | AMForm [AForm]

unSTrapezed = fromJust $ trapezed

anyTBSBPForm :: [Int] -> [(Float,Float)]
anyTBSBPForm (p0:p1:[]) = [anyTBSBPVector p0 p1]
anyTBSBPForm (p0:p1:l)  = (anyTBSBPVector p0 p1) : (anyTBSBPForm (p1:l))
anyTBSBPForm _ = []

-- trapezBSBP
anyTBSBPVector :: Int -> Int -> (Float,Float)
anyTBSBPVector p0 p1 = (\ (x0,y0) (x1,y1) -> (x1-x0,y1-y0)) (trapezBSBP p0, trapezBSBP p1)
-}
-- не включает меньшее основание трапеции
trapezBSBP :: Int -> (Float,Float) 
trapezBSBP 1 = (4,4) -- 1,9,14,19 - пересечение наибольшей грани
trapezBSBP 2 = bsbRSL 2 3
trapezBSBP 3 = (2,2)
trapezBSBP 4 = bsbRSL 1 3
trapezBSBP 5 = bsbRSR 2 3
trapezBSBP 6 = bsbRR  1 2
trapezBSBP 7 = bsbRSL 2 3
trapezBSBP 8 = bsbRSR 2 2
trapezBSBP i | i > 0 = let 
	ki = mod i 10
	mn5 h = (i - h) `div` 5
	ifI :: Int -> Int
	ifI k | or[k == 4,k == 9] = 1
	ifI k | or[k == 5,k == 0] = 2
	ifI k | or[k == 6,k == 1] = 3 
	ifI k | or[k == 7,k == 2] = 4
	ifI k | or[k == 8,k == 3] = 5
	in case ifI ki of
		1 -> bsbRSL (((2+) . mn5) 9) 4 --  1,9,14,19 - пересечение наибольшей грани
		2 -> bsbRSR (((2+) . mn5) 10) 3 -- 5,10,15 Sr and R
		3 -> bsbRR  1 (((2+) . mn5) 11) -- 6,11,21 R and R
		4 -> bsbRSL (((2+) . mn5) 12) 3 -- 7,12,17 Sl and R
		5 -> bsbRSR (((2+) . mn5) 13) 2 -- 8,13,18 R and R


anyTBSBP :: Int -> Int -> (Float,Float)
anyTBSBP p i = (\(x,y)->(x*2**(intToFloat p),y*2**(intToFloat p))) (trapezBSBP i)

anyTBSBPl :: [(Int,Int)] -> [(Float,Float)]
anyTBSBPl = map (\ (x,y) -> anyTBSBP x y)

fi :: [Float] -> (Float,Float) -> (Float -> Float)
fi l (xi,yi) x = (yi *) $! (zn l )/(ch l)
	where
		zn [] = 1
		zn (xj:xs) = (x-xj) * (zn xs)
		ch :: [Float] -> Float
		ch [] = 1
		ch (xj':xs') = (xi - xj') * (ch xs')

fx :: [(Float,Float)] -> (Float -> Float)
fx (x0:l) x = fl lin
	where
		lin = LLine [] x0 l
		fl :: LLine (Float,Float) -> Float
		fl (LLine l pi []) = fi (map fst l) pi x
		fl (LLine l1 pi l2) = (fi ((map fst l1)++(map fst l2)) pi x) + (fl (LLine (pi:l1) (head l2) (tail l2)))

data Body = Body [Int] Int [Int] Int
type Clos = Body 

getLPoint :: Body -> [(Float,Float)] -- Simple unFull
getLPoint (Body p1 p2 p3 p4) = (map (anyTBSBP 1) )$ (map t3_8ToAny p1) ++ [t2_7ToAny p2] ++ (map t1_6_0_5ToAny p3) ++ [t1_9ToAny p4]
	where
		trans10 :: Int -> Int -> Int -> Int
		trans10 n1 n2 n = (10*d2) + (if m2 == 0 then n1 else n2)
			where
				m2 = mod n 2
				d2 = div n 2 
		t3_8ToAny :: Int -> Int
		t3_8ToAny = trans10 3 8
		t2_7ToAny :: Int -> Int
		t2_7ToAny = trans10 2 7
		t1_6_0_5ToAny :: Int -> Int
		t1_6_0_5ToAny n = (10*d4) + (case m4 of
				0 -> (10*(if d4 <= 0 then 1 else d4))
				1 -> (10*d4) + 1
				2 -> (10*d4) + 5
				3 -> (10*d4) + 6
			)
			where
				m4 = mod n 4
				d4 = div n 4
		t1_9ToAny :: Int -> Int
		t1_9ToAny = trans10 1 9

getFXTBSBP_Clos_Simple :: Clos -> Body -> (Float -> Float)
getFXTBSBP_Clos_Simple cl bd = fx ((map (\(x,y)->(x/2,y/2)) clP) ++ bdP ++ (map (\(x,y)->(x*2,y*2)) clP))
	where
		clP = getLPoint cl
		bdP = getLPoint bd

drowFXTBSBP_Clos_Simple :: Float -> Int -> Int -> IO Picture
drowFXTBSBP_Clos_Simple eps maxI i = do
	cl <- getRB
	bd <- getRB
	return $ Line $ inTrapez $ getFXTBSBP_Clos_Simple cl bd
	where
		getRB :: IO Body
		getRB = do
			lp1 <- sequence $ (take i) $ repeat $ randomRIO (1,maxI)
			p2 <- randomRIO (1,maxI)
			lp3 <- sequence $ (take i) $ repeat $ randomRIO (1,maxI)
			p4 <- randomRIO (1,maxI)
			return $ Body lp1 p2 lp3 p4
		inTrapez :: (Float -> Float) -> [(Float,Float)]
		inTrapez f = getLine (2,4)
			where
			getLine :: (Float,Float) -> [(Float,Float)]
			getLine (l,r) = appF l
				where
					ifY y x | y < 0 = 0 
						  	| y > x = x
					ifY y _ = y
					appF :: Float -> [(Float,Float)]
					appF x | x < r = (x,ifY (f x) x) : (appF (x + eps))
					appF _ = []

getFXTBSBPunFullG :: Int -> Int -> Int -> IO (Float -> Float)
getFXTBSBPunFullG i stP maxI = do
	let lp = [stP,stP+1..]
	il <- sequence $ (take i) $ repeat $ randomRIO (1,maxI)
	return $! fx $ anyTBSBPl (zip lp il)

randomDrowFXTBSBPunFullG :: Float -> Int -> (Int,Int) -> (Int,Int) -> IO Picture
randomDrowFXTBSBPunFullG eps i powFX deep = do
	pl <- sequence $ (take i) $ repeat $ randomRIO powFX
	dl <- sequence $ (take i) $ repeat $ randomRIO deep
	drowFXTBSBPunFullG eps (zip pl dl)  

drowFXTBSBPunFullG :: Float -> [(Int,Int)] -> IO Picture
drowFXTBSBPunFullG eps lParFX = fmap (Pictures . map Line . map inTrapez) $ sequence $! map (\(x,y) -> getFXTBSBPunFullG x 0 y) lParFX -- :)
	where
		inTrapez :: (Float -> Float) -> [(Float,Float)]
		inTrapez f = getLine (2,4)
			where
				getLine :: (Float,Float) -> [(Float,Float)]
				getLine (l,r) = appF l
					where
						ifY y x | y < 0 = 0 
							  	| y > x = x
						ifY y _ = y
						appF :: Float -> [(Float,Float)]
						appF x | x < r = (x,ifY (f x) x) : (appF (x + eps))
						appF _ = []

drowFXBSBPunFullG :: Int ->  Float -> [(Int,Int,Int)] -> IO Picture
drowFXBSBPunFullG endP eps lParFX = fmap (Pictures . map Line . map inTrapez) $ sequence $! map (\(x,y,z) -> getFXTBSBPunFullG x y z) lParFX -- :)
	where
		inTrapez :: (Float -> Float) -> [(Float,Float)]
		inTrapez f = getLine (2,2^^endP)
			where
				getLine :: (Float,Float) -> [(Float,Float)]
				getLine (l,r) = appF l
					where
						appF :: Float -> [(Float,Float)]
						appF x | x < r = (x,f x) : (appF (x + eps))
						appF _ = []

drowTBSBP :: Int -> (Int,Int) -> (Int,Int) -> IO Picture
drowTBSBP i pp pi = do
	pl <- sequence $ (take i) $ repeat $ randomRIO pp
	il <- sequence $ (take i) $ repeat $ randomRIO pi
	let lpoint = map meybePoint $ anyTBSBPl (zip pl il)
	return $ Line lpoint
	where
		ifX x | x > 4 = 4
			  | x < 2 = 2
		ifX x = x	  
		ifY y x | y < 0 = 0 
			  	| y > x = x
		ifY y _ = y
		meybePoint :: (Float,Float) -> (Float,Float)
		meybePoint (x,y) = (ifX x, ifY y x)