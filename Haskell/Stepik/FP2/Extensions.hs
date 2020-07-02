{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

class Functor' c e | c -> e where
    fmap' :: (e -> e) -> c -> c

instance Functor f => Functor' (f a) a where
    fmap' = fmap
