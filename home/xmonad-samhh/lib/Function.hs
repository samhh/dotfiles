module Function (($.), if2, (<$<), bindM2) where

-- | Blackbird operator for composition over two arguments.
($.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
($.) = (.) . (.)

-- | Predicate and two branches on two arguments.
if2 :: (a -> b -> Bool) -> (a -> b -> c) -> (a -> b -> c) -> a -> b -> c
if2 p f g x y = if p x y then f x y else g x y

-- | Compose two functions where the first returns a functor and the second is
-- to be applied within said functor.
(<$<) :: Functor f => (b -> c) -> (a -> f b) -> a -> f c
(<$<) = fmap . fmap

-- | Monadic bind over a binary function.
bindM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f g h = join $ liftA2 f g h

