module EgorList (EgorList) where

data EgorList a = Cons a (EgorList a) | HappyEnd

instance (Show a) => Show (EgorList a) where
  show HappyEnd = "()"
  show (Cons a b) = "(" ++ show a ++ " " ++ show b ++ ")"

append :: EgorList a -> EgorList a -> EgorList a
append (Cons a HappyEnd) b = Cons a b
append HappyEnd b = b
append (Cons a as) b = Cons a (append as b)

instance Functor EgorList where
  --fmap :: (a -> b) -> f a -> f b
  fmap _ HappyEnd = HappyEnd
  fmap f (Cons x ys) = Cons (f x) (fmap f ys)

  --(<$) :: a -> f b -> f a
  (<$) _ HappyEnd = HappyEnd
  (<$) a (Cons x xs) = Cons a (a <$ xs)

instance Applicative EgorList where
  -- pure :: a -> f a
  pure a = Cons a HappyEnd

  --(<*>) :: f (a -> b) -> f a -> f b
  (<*>) _ HappyEnd = HappyEnd
  (<*>) HappyEnd _ = HappyEnd
  (<*>) (Cons f fs) (Cons x xs) = append (fmap f (Cons x xs)) (fs <*> (Cons x xs))
