{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    PatternGuards, GeneralizedNewtypeDeriving, OverloadedStrings,
    DeriveTraversable, DeriveFunctor, ViewPatterns #-}

module Text.Pandoc.Readers.Docx.Reducible2 ( Reducible(..)
                                           , RInlines
                                           , RBlocks
                                           , red)
       where


import Text.Pandoc.Builder
-- import Text.Pandoc.Shared (extractSpaces)
import Data.Monoid
import Data.List
import Data.Sequence (ViewR(..), ViewL(..), viewl, viewr)
import qualified Data.Sequence as Seq (null)
import Control.Applicative
import Data.String
import Data.Traversable
import Data.Foldable (Foldable)
import Control.Applicative

newtype Modifier a = Modifier (Maybe (Transformation a))

type Transformation a = a -> a

unMod :: Modifier a -> Maybe (Transformation a)
unMod (Modifier f) = f

instance (Monoid a, Eq a) => (Eq  (Transformation a)) where
  f == g = f mempty == g mempty

modifier :: (a -> a) -> Modifier a
modifier f = Modifier (Just f)

class (Eq a) => Modifiable a where
  getModifier :: a -> Modifier a
  innards :: a -> a
  spaceOut :: a -> a
  spaceOut = id

instance (Monoid a, Show a) => Show (Modifier a) where
  show (Modifier f) = show  (f <*> Just mempty)

instance (Monoid a, Eq a) => Eq (Modifier a) where
  (Modifier f) == (Modifier g) = f == g

instance Modifiable Inlines where
  getModifier ils = case viewl (unMany ils) of
    (x :< xs) | Seq.null xs -> case x of
      (Emph _)        -> modifier emph
      (Strong _)      -> modifier strong
      (SmallCaps _)   -> modifier smallcaps
      (Strikeout _)   -> modifier strikeout
      (Superscript _) -> modifier superscript
      (Subscript _)   -> modifier subscript
      (Span attr _)   -> modifier (spanWith attr)
      _               -> Modifier Nothing
    _ -> Modifier Nothing

  innards ils = case viewl (unMany ils) of
    (x :< xs) | Seq.null xs -> case x of
      (Emph lst)        -> fromList lst
      (Strong lst)      -> fromList lst
      (SmallCaps lst)   -> fromList lst
      (Strikeout lst)   -> fromList lst
      (Superscript lst) -> fromList lst
      (Subscript lst)   -> fromList lst
      (Span _ lst)      -> fromList lst
      _        -> ils
    _          -> ils

  -- spaceOut ils = extractSpaces (stack fs) ils'
  --   where (fs, ils') = unstack ils

  spaceOut ils =
    let (fs, ils') = unstack ils
        contents = unMany ils'
        left  = case viewl contents of
          (Space :< _) -> space
          _            -> mempty
        right = case viewr contents of
          (_ :> Space) -> space
          _            -> mempty in
    (left <>
     (stack fs $ trimInlines . Many $ contents)
     <> right)

instance Modifiable Blocks where
  getModifier blks = case viewl (unMany blks) of
    (x :< xs) | Seq.null xs -> case x of
      (BlockQuote _) -> modifier blockQuote
      (Div attr _)   -> modifier (divWith attr)
      _               -> Modifier Nothing
    _ -> Modifier Nothing

  innards blks = case viewl (unMany blks) of
    (x :< xs) | Seq.null xs -> case x of
      (BlockQuote lst) -> fromList lst
      (Div attr lst)   -> fromList lst
      _        -> blks
    _          -> blks

-- unfold
unstack :: (Modifiable a) => a -> ([Transformation a], a)
unstack ms = unfoldAccum uf ms []
  where
    uf b = flip (,) (innards b) <$> unMod (getModifier b)

-- fold
stack :: (Modifiable a) => [Transformation a] -> a -> a
stack [] ms = ms
stack (f : fs) ms = f $ stack fs ms

isEmpty :: (Monoid a, Eq a) => a -> Bool
isEmpty x = x == mempty

combine :: (Monoid a, Modifiable a, Eq a) => a -> a -> a
combine x y =
  let (xfs, xs) = unstack x
      (yfs, ys) = unstack y
      shared = xfs `intersect` yfs
      x_remaining = xfs \\ shared
      y_remaining = yfs \\ shared
  in
   case null shared of
     True  | isEmpty xs && isEmpty ys -> mempty
           | isEmpty xs -> stack y_remaining y
           | isEmpty ys -> stack x_remaining x
           | otherwise -> (spaceOut x) <> (spaceOut y)
     False -> stack shared $
              combine
              (stack x_remaining xs)
              (stack y_remaining ys)

(<+>) :: (Monoid a, Modifiable a, Eq a) => a -> a -> a
x <+> y = combine x y

concatReduce :: (Monoid a, Modifiable a) => [a] -> a
concatReduce xs = foldl combine mempty xs

newtype Reducible a = Reducible {unReduce :: a}
                    deriving (Show, Eq)

red :: (Modifiable a) => a -> Reducible a
red = Reducible

instance (Monoid a, Modifiable a) => Monoid (Reducible a) where
  mappend r s = Reducible (unReduce r `combine` unReduce s)
  mempty = Reducible mempty

instance Functor Reducible where
  fmap f r = Reducible $ f $ unReduce r

instance Applicative Reducible where
  pure = Reducible
  (<*>) f r = Reducible $ (unReduce f) (unReduce r)

type RInlines = Reducible Inlines

type RBlocks = Reducible Blocks

instance IsString (RInlines) where
  fromString = (red . text)

unfoldAccum      :: (b -> Maybe (a, b)) -> b -> [a] -> ([a], b)
unfoldAccum f b acc  =
  case f b of
    Just (a,new_b) ->  unfoldAccum f new_b (a:acc)
    Nothing        -> (acc, b)
