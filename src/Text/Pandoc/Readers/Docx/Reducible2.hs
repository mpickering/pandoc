{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    PatternGuards, GeneralizedNewtypeDeriving, OverloadedStrings #-}

import Text.Pandoc.Builder
import Data.Monoid
import Data.List
import qualified Data.Sequence as S
import Control.Applicative
import Data.String

data Modifier a = Modifier (a -> a)
                | AttrModifier (Attr -> a -> a) Attr
                | NullModifier

class (Eq a) => Modifiable a where
  modifier :: a -> Modifier a
  innards :: a -> a

  spaceOutL :: a -> a
  spaceOutL = id

  spaceOutR :: a -> a
  spaceOutR = id
    
instance (Monoid a, Show a) => Show (Modifier a) where
  show (Modifier f) = show $ f mempty
  show (NullModifier) = "NullModifier"

instance (Monoid a, Eq a) => Eq (Modifier a) where
  (Modifier f) == (Modifier g) = (f mempty == g mempty)

instance Modifiable Inlines where
  modifier ils = case S.viewl (unMany ils) of
    (x S.:< xs) | S.null xs -> case x of
      (Emph _)        -> Modifier emph
      (Strong _)      -> Modifier strong
      (SmallCaps _)   -> Modifier smallcaps
      (Strikeout _)   -> Modifier strikeout
      (Superscript _) -> Modifier superscript
      (Subscript _)   -> Modifier superscript
      (Span attr _)   -> AttrModifier spanWith attr
      _               -> NullModifier
    _ -> NullModifier

  innards ils = case S.viewl (unMany ils) of
    (x S.:< xs) | S.null xs -> case x of
      (Emph lst) -> fromList lst
      (Strong lst) -> fromList lst
      _        -> ils
    _          -> ils

  spaceOutL ils =
    let (fs, xs) = unstack ils
    in
     case S.viewl (unMany xs) of
       (Space S.:< seq) -> space <> (spaceOutL $ stack fs $ Many seq)
       _               -> ils

  spaceOutR ils =
    let (fs, xs) = unstack ils
    in
     case S.viewr (unMany xs) of
       (seq S.:> Space) -> (spaceOutR $ stack fs $ Many seq) <> space
       _               -> ils

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
           | otherwise -> (spaceOutR x) <> (spaceOutL y)
     False -> stack shared $
              combine 
              (stack x_remaining xs) 
              (stack y_remaining ys)

newtype Reducible a = Reducible {unReduce :: a}
                    deriving (Show, Eq)

red :: (Modifiable a) => a -> Reducible a
red = Reducible

instance IsString (Reducible Inlines) where
  fromString = (red . text)

instance (Monoid a, Modifiable a) => Monoid (Reducible a) where
  mappend r s = Reducible (unReduce r `combine` unReduce s)
  mempty = Reducible mempty

instance Functor Reducible where
  fmap f r = Reducible $ f $ unReduce r

instance Applicative Reducible where
  pure = Reducible
  (<*>) f r = Reducible $ (unReduce f) (unReduce r)
