module Data.Datum where

-- | Isomorphic to a tuple `(a, a)`, a `Datum` represents the present and
-- previous values of `a`.
data Datum a = Datum
  { prev :: a
  , curr :: a
  }

instance Semigroup a => Semigroup (Datum a) where
  x <> y = Datum (prev x <> prev y) (curr x <> curr y)

instance Monoid a => Monoid (Datum a) where
  mempty = Datum mempty mempty

renew :: a -> Datum a -> Datum a
renew x y = Datum (curr y) x
