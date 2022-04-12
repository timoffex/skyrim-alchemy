{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Data.HList where


data HList xs where
  HEmpty :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)


class Has x xs where
  get :: HList xs -> x


instance Has x (x ': xs) where
  get (HCons x _) = x

-- Marking this INCOHERENT makes the above instance take precedence, so that
-- 'get' always returns the first instance of the requested type.
instance {-# INCOHERENT #-} Has x xs => Has x (a ': xs) where
  get (HCons _ xs) = get xs