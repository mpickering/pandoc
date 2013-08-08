{-# LANGUAGE CPP #-}
module Text.Pandoc.Compat.Monoid ( Monoid(..)
                                 , (<>)
                          ) where

#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>), Monoid(..))
#else
import Data.Monoid (mappend, Monoid(..))
#endif

#if MIN_VERSION_base(4,5,0)
#else
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif
