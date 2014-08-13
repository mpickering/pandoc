{-# LANGUAGE FlexibleInstances, FlexibleContexts, PatternGuards #-}

import Text.Pandoc.Builder
import Data.Monoid
import Data.List (intersect, (\\))
import qualified Data.Sequence as S

data Modifier a = SimpleModifier (a -> a)
           | AttrModifier (Attr -> a -> a) Attr
           | NullModifier

instance (Show a) => Show (Modifier (Many a)) where
  show (SimpleModifier x) =
    reverse $ drop 3 $ reverse $ show $ head $ toList $ x $ fromList []
  show (AttrModifier x attr) =
    reverse $ drop 3 $ reverse $ show $ head $ toList $ x attr $ fromList []
  show (NullModifier) = "NullContainer"

instance (Monoid a, Eq a) => Eq (Modifier a) where
  (SimpleModifier x) == (SimpleModifier y) =
    (x mempty) == (y mempty)
  (AttrModifier x attr) == (AttrModifier y attr') =
    (x attr mempty) == (y attr' mempty)
  NullModifier == NullModifier = True
  _ == _ = False

class (Monoid (Many a), Eq (Many a), Eq a) => Modifiable a where
  modifier :: Many a -> Modifier (Many a)
  innards :: Many a -> Many a
  isSpc :: a -> Bool
  isSpc _ = False

instance Modifiable Inline where
  modifier ms = case S.viewl (unMany ms) of
    (x S.:< xs) | S.null xs -> case x of
      (Emph _) -> SimpleModifier emph
      (Strong _) -> SimpleModifier strong
      (SmallCaps _) -> SimpleModifier smallcaps
      (Strikeout _) -> SimpleModifier strikeout
      (Subscript _) -> SimpleModifier subscript
      (Superscript _) -> SimpleModifier superscript
      (Span attr _) -> AttrModifier spanWith attr
      _             -> NullModifier
    _  -> NullModifier

  innards ms = case S.viewl (unMany ms) of
    (x S.:< xs) | S.null xs -> case x of
      (Emph ils) -> fromList ils
      (Strong ils) -> fromList ils
      (SmallCaps ils) -> fromList ils
      (Strikeout ils) -> fromList ils
      (Subscript ils) -> fromList ils
      (Superscript ils) -> fromList ils
      (Span _ ils) -> fromList ils
      _             -> mempty
    _ -> ms

  isSpc (Space) = True
  isSpc _       = False

instance Modifiable Block where
  modifier ms = case S.viewl (unMany ms) of
    (x S.:< xs) | S.null xs -> case x of
      (BlockQuote _) -> SimpleModifier blockQuote
      (Div attr _) -> AttrModifier divWith attr
      _            -> NullModifier

  innards ms = case S.viewl (unMany ms) of
    (x S.:< xs) | S.null xs -> case x of
      (BlockQuote bs) -> fromList bs
      (Div _ bs) -> fromList bs
      _          -> mempty
    _            -> ms

sepModifiers :: (Modifiable a) => Many a -> ([Modifier (Many a)], (Many a))
sepModifiers ms | NullModifier <- modifier ms = ([], ms)
sepModifiers ms = (modf : modfs, ms')
  where modf = modifier ms
        (modfs, ms') = sepModifiers $ innards ms

applyModifiers :: (Modifiable a) => [Modifier (Many a)] -> Many a -> Many a
applyModifiers [] ms = ms
applyModifiers (NullModifier:mfs) ms = applyModifiers mfs ms
applyModifiers ((SimpleModifier mf):mfs) ms = applyModifiers mfs $ mf ms
applyModifiers ((AttrModifier mf attr):mfs) ms = applyModifiers mfs $ mf attr ms

spaceOutL :: (Modifiable a) => Many a -> Many a
spaceOutL r | (xfs, xs) <- sepModifiers r
            , (y S.:< ys) <- S.viewl $ unMany xs
            , isSpc y = singleton y <> (applyModifiers xfs $ Many ys)
spaceOutL r = r

spaceOutR :: (Modifiable a) => Many a -> Many a
spaceOutR r | (xfs, xs) <- sepModifiers r
            , (ys S.:> y) <- S.viewr $ unMany xs
            , isSpc y = (applyModifiers xfs $ Many ys) <> (singleton y)
spaceOutR r = r

combineManys :: (Modifiable a) => Many a -> Many a -> Many a
combineManys x y =
  let (xfs, xs) = sepModifiers x
      (yfs, ys) = sepModifiers y
      shared = xfs `intersect` yfs
      x_remaining = xfs \\ shared
      y_remaining = yfs \\ shared
  in
   case null shared of
     True  | isNull xs && isNull ys -> mempty
           | isNull xs -> applyModifiers y_remaining y
           | isNull ys -> applyModifiers x_remaining x
           | otherwise -> (spaceOutR x) <> (spaceOutL y)
     False -> applyModifiers shared $
               (applyModifiers x_remaining xs) <++>
               (applyModifiers y_remaining ys)

newtype Reducible a = Reducible {unReduce :: Many a}
             deriving (Eq, Show)

reducible :: (Modifiable a) => Many a -> Reducible a
reducible = Reducible

instance (Modifiable a) => Monoid (Reducible a) where
  mempty = Reducible $ fromList []
  mappend r s = case () of
    _ | S.EmptyR <- S.viewr $ unMany $ unReduce r -> s
      | S.EmptyL <- S.viewl $ unMany $ unReduce s -> r
      | (xs S.:> x) <- S.viewr $ unMany $ unReduce r,
        (y S.:< ys) <- S.viewl $ unMany $ unReduce s ->
          Reducible $ (Many xs) <>
                      (combineManys (fromList [x]) (fromList [y])) <>
                      (Many ys)

type InlinesR = Reducible Inline
type BlocksR  = Reducible Block

singletonR :: a -> Reducible a
singletonR a = Reducible $ singleton a

toListR :: Reducible a -> [a]
toListR rs = toList $ unReduce rs

fromListR :: [a] -> Reducible a
fromListR as = Reducible $ fromList as

isNullR :: Reducible a -> Bool
isNullR rs = isNull $ unReduce rs

docR :: BlocksR -> Pandoc
docR bs = doc $ unReduce bs

-- This allows us to combine manys and perform proper reductions,
-- without having to work with a Reducible
(<++>) :: (Modifiable a) => Many a -> Many a -> Many a
xs <++> ys = unReduce $ reducible xs <> reducible ys
