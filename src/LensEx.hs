{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module LensEx where

import           Control.Lens

import           Control.Monad.State
import           Data.List


data Point = Point
  { _positionX   :: Double
    , _positionY :: Double
  } deriving (Show)
-- makeLenses ''Point

data Segment = Segment
  { _segmentStart :: Point
    , _segmentEnd :: Point
  } deriving (Show)
-- makeLenses ''Segment

makePoint :: (Double, Double) -> Point
makePoint (x, y) = Point x y

unmakePoint :: Point -> (Double, Double)
unmakePoint (Point x y) = (x,y)

makeSegment
  :: (Double, Double) -> (Double, Double) -> Segment
makeSegment start end = Segment (makePoint start) (makePoint end)

pointCoordinates
  :: Applicative f => (Double -> f Double) -> Point -> f Point
pointCoordinates g (Point x y) = Point <$> g x <*> g y

deleteIfNegative :: Double -> Maybe Double
deleteIfNegative x =  if x < 0 then Nothing else Just x

extremityCoordinates
  :: Applicative f => (Double -> f Double) -> Segment -> f Segment
extremityCoordinates g (Segment start end) = Segment <$> pointCoordinates g start <*> pointCoordinates g end

scaleSegment :: Double -> Segment -> Segment
scaleSegment n = over extremityCoordinates (* n)

mymapped
  :: Functor f => (a -> Identity b) -> f a -> Identity (f b)
mymapped f = Identity . fmap (runIdentity . f)

positionX
  :: Functor f => (Double -> f Double) -> Point -> f Point
positionX k p = (\x -> p { _positionX = x }) <$> k (_positionX p)

positionY
  :: Functor f => (Double -> f Double) -> Point -> f Point
positionY k p = (\x -> p { _positionY = x }) <$> k (_positionY p)

segmentStart
  :: Functor f => (Point -> f Point) -> Segment -> f Segment
segmentStart g s = (\p -> s { _segmentStart = p }) <$> g (_segmentStart s)

segmentEnd
  :: Functor f => (Point -> f Point) -> Segment -> f Segment
segmentEnd g s = (\p -> s { _segmentEnd = p }) <$> g (_segmentEnd s)

mylens
  :: Functor f => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
mylens getter setter = \k x -> setter x <$> k (getter x)


stateExample :: State Segment ()
stateExample = do
    segmentStart .= makePoint (0,0)
    zoom segmentEnd $ do
        positionX += 1
        positionY *= 2
        pointCoordinates %= negate

-- ΠΣ: execState stateExample (makeSegment (1, 2) (5, 3))
-- Segment {_segmentStart = Point {_positionX = 0.0, _positionY = 0.0}, _segmentEnd = Point {_positionX = -6.0, _positionY = -6.0}}

pointPair :: Iso' Point (Double, Double)
pointPair = iso unmakePoint makePoint

prefixed :: Eq a => [a] -> Prism' [a] [a]
prefixed prefix = prism' (prefix ++) (stripPrefix prefix)
-- ΠΣ: preview (prefixed "hello") "hello world"
-- Just " world"


-- Prefix                       Infix
-- view _1 (1,2)                (1,2) ^. _1
-- set _1 7 (1,2)               (_1 .~ 7) (1,2)
-- over _1 (2 *) (1,2)          (_1 %~ (2 *)) (1,2)
-- toListOf traverse [1..4]     [1..4] ^.. traverse
-- preview traverse []          [] ^? traverse



-- type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- outside 
--   :: Prism s t a b -> Lens (t -> r) (s -> r) (b -> r) (a -> r)
myEither :: (a -> c) -> (b -> c) -> Either a b -> c
myEither f g 
  = outside _Left .~ f
  $ outside _Right .~ g
  $ error "Impossible" -- There is no sensible default here.


myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe def f
    = outside _Just .~ f
    $ const def -- A default Maybe a -> b function.

data SomeData = Foo Int 
              | Bar Char

-- Will create prisms named _Foo and _Bar
$(makePrisms ''SomeData)

functionToOverride :: SomeData -> Int
functionToOverride = const 5

-- If the arg is a Foo, return the contained int + 1 
newFunction :: SomeData -> Int
newFunction = functionToOverride & outside _Foo .~ succ