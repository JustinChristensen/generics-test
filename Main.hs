{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Typeable
import GHC.Generics
import Control.Applicative ((<|>))
import qualified Foo as F
import qualified Bar as B

class GetFieldG f where
    getFieldG :: Typeable b => String -> f p -> Maybe b

instance GetFieldG V1 where
    getFieldG _ x = case x of {}

instance GetFieldG U1 where
    getFieldG _ _ = Nothing

instance (GetFieldG f, GetFieldG g) => GetFieldG (f :+: g) where
    getFieldG s (L1 x) = getFieldG s x
    getFieldG s (R1 x) = getFieldG s x

instance (GetFieldG f, GetFieldG g) => GetFieldG (f :*: g) where
    getFieldG s (f :*: g) = getFieldG s f <|> getFieldG s g

instance GetFieldG (K1 i c) where
    getFieldG _ (K1 _) = Nothing

instance GetFieldG f => GetFieldG (D1 d f) where
    getFieldG s (M1 x) = getFieldG s x

instance GetFieldG f => GetFieldG (C1 c f) where
    getFieldG s (M1 x) = getFieldG s x

instance (Typeable b, Selector s, GetFieldG (K1 i b)) => GetFieldG (S1 s (K1 i b)) where
    getFieldG s (M1 x) | s == field = cast $ unK1 x
                       | otherwise = getFieldG s x
        where field = selName (undefined :: t s (K1 i b) p)

class GetField a where
    getField :: Typeable b => String -> a -> Maybe b
    default getField :: (Typeable b, Generic a, GetFieldG (Rep a)) => String -> a -> Maybe b
    getField s x = getFieldG s $ from x

instance GetField F.Foo
instance GetField B.Bar

foo :: F.Foo
foo = F.Foo "foo works" 9000

bar :: B.Bar
bar = B.Bar True "bar works"

main :: IO ()
main = do
    print (getField "foo" foo :: Maybe String)
    print (getField "foo" bar :: Maybe String)
    print (getField "bar" foo :: Maybe Int)