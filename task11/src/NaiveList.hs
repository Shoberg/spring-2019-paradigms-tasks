{-|
  Реализация класса типов 'Map' в виде односвязного списка,
  работает за линейное время в худшем случае.
-}
module NaiveList where
import Map

-- |Односвязный список, хранящий отсортированные пары "ключ-значение".
data NaiveList k a = Nil | Cons k a (NaiveList k a) deriving (Show, Eq)

{-|
  Реализация функций 'Map' для 'NaiveList'.
  'Map.null' реализуется отдельно, чтобы работать за
  /O(1)/, а не /O(n)/, несмотря на то, что его реализация
  по умолчанию доступна в 'Map'.
-}
instance Map NaiveList where
    empty = Nil

    singleton k a = Cons k a Nil

    toAscList Nil           = []
    toAscList (Cons k a xs) = (k, a) : toAscList xs

    alter f k Nil =
        case f Nothing of
        Nothing -> empty
        Just a  -> singleton k a
    alter f k    (Cons k' a' xs) | k' < k = Cons k' a' (alter f k xs)
    alter f k xs@(Cons k' _  _ ) | k' > k = maybe xs (\a -> Cons k a xs) $ f Nothing
    alter f k    (Cons _  a' xs)          = maybe xs (\a -> Cons k a xs) $ f (Just a')

    lookup _ Nil = Nothing
    lookup k (Cons k' _ xs) | k' < k = Map.lookup k xs
    lookup k (Cons k' _ _)  | k' > k = Nothing
    lookup _ (Cons _  a _)           = Just a

    null Nil = True
    null _   = False

    size Nil           = 0
    size (Cons _ _ xs) = 1 + size xs
