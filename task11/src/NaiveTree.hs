{-|
  Реализация класса типов 'Map' в виде дерева поиска,
  необязательно сбалансированного, работает за линейное
  время в худшем случае.
-}
module NaiveTree where
import Map

{-|
  Двоичное дерево поиска, необязательно сбалансированное.

  Инвариант: для любой вершины @v@:

  1. Все ключи в левом поддереве строго меньше ключа @v@.
  2. Все ключи в правом поддереве строго больше ключа @v@.
-}
data NaiveTree k a =
    -- |Пустое дерево
    Nil
    -- |@Node k a l r@ – дерево с корнем в вершине с ключом @k@,
    -- значением @a@, левым ребёнком @l@ и правым ребёнком @r@.
  | Node k a (NaiveTree k a) (NaiveTree k a)
  deriving (Show, Eq)

{-|
  @merge l r@ объединяет два дерева в одно при условии,
  что все ключи из @l@ строго меньше ключей из @r@.
-}
merge :: NaiveTree k a -> NaiveTree k a -> NaiveTree k a
merge Nil right = right
merge (Node k a l r) right = Node k a l (merge r right)

{-|
  Реализация функций 'Map' для 'NaiveTree'.

  'empty', 'singleton' и 'Map.null' работают за /O(1)/.
  Если /n/ – количество вершин дерева, а /h/ – высота дерева,
  то 'fromList' работает за /O(nh)/, 'toAscList' работает за /O(n^2)/,
  а 'size' работает за /O(n)/.
  Остальные функции работают за /O(h)/,
  причём каждая функция должна спускаться вниз по дереву и
  подниматься обратно не больше одного раза.

  Скорее всего, при реализации вам потребуется функция 'merge'.
-}
instance Map NaiveTree where
    empty = Nil

    singleton k a = Node k a Nil Nil

    toAscList Nil            = []
    toAscList (Node k a l r) = toAscList l ++ [(k, a)] ++ toAscList r

    alter f k Nil = maybe empty (singleton k) $ f Nothing
    alter f key (Node k a l r)
        | key < k   = alter f key l `merge` Node k a Nil Nil `merge` r
        | key > k   = l `merge` Node k a Nil Nil `merge` alter f key r
        | otherwise = case f (Just a) of 
            Just value -> l `merge` Node k value Nil Nil `merge` r
            Nothing    -> l `merge` r

    lookup _ Nil = Nothing
    lookup key (Node k a l r)
        | key < k   = Map.lookup key l
        | key > k   = Map.lookup key r
        | otherwise = Just a

    null Nil = True
    null _   = False

    size Nil = 0
    size (Node _ _ l r) = 1 + size l + size r
