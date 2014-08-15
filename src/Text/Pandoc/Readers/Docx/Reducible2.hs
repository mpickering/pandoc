{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    PatternGuards, GeneralizedNewtypeDeriving, OverloadedStrings,
    DeriveTraversable #-}

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

data Modifier a = Modifier (a -> a)
                | AttrModifier (Attr -> a -> a) Attr
                | NullModifier

class (Eq a) => Modifiable a where
  modifier :: a -> Modifier a
  innards :: a -> a
  spaceOut :: a -> a
  spaceOut = id

instance (Monoid a, Show a) => Show (Modifier a) where
  show (Modifier f) = show $ f mempty
  show (NullModifier) = "NullModifier"

instance (Monoid a, Eq a) => Eq (Modifier a) where
  (Modifier f) == (Modifier g) = (f mempty == g mempty)
  (AttrModifier f attr) == (AttrModifier g attr') = (f attr mempty == g attr' mempty)
  (NullModifier) == (NullModifier) = True
  _ == _ = False

instance Modifiable Inlines where
  modifier ils = case viewl (unMany ils) of
    (x :< xs) | Seq.null xs -> case x of
      (Emph _)        -> Modifier emph
      (Strong _)      -> Modifier strong
      (SmallCaps _)   -> Modifier smallcaps
      (Strikeout _)   -> Modifier strikeout
      (Superscript _) -> Modifier superscript
      (Subscript _)   -> Modifier subscript
      (Span attr _)   -> AttrModifier spanWith attr
      _               -> NullModifier
    _ -> NullModifier

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
  modifier blks = case viewl (unMany blks) of
    (x :< xs) | Seq.null xs -> case x of
      (BlockQuote _) -> Modifier blockQuote
      (Div attr _)   -> AttrModifier divWith attr
      _               -> NullModifier
    _ -> NullModifier

  innards blks = case viewl (unMany blks) of
    (x :< xs) | Seq.null xs -> case x of
      (BlockQuote lst) -> fromList lst
      (Div attr lst)   -> fromList lst
      _        -> blks
    _          -> blks

unstack :: (Modifiable a) => a -> ([Modifier a], a)
unstack ms = case modifier ms of
  NullModifier -> ([], ms)
  _            -> (f : fs, ms') where
    f = modifier ms
    (fs, ms') = unstack $ innards ms

stack :: (Modifiable a) => [Modifier a] -> a -> a
stack [] ms = ms
stack (NullModifier : fs) ms = stack fs ms
stack ((Modifier f) : fs) ms = f $ stack fs ms
stack ((AttrModifier f attr) : fs) ms = f attr $ stack fs ms

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
