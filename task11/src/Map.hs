{-|
  Определение класса типов 'Map'.
-}
module Map where

{-|
  Поведение всех определённых здесь функций должно быть аналогично
  поведению функций из модуля "Data.Map.Strict".

  Каждую функцию, у которой здесь предложена реализация по умолчанию
  в виде 'undefined', вам требуется выразить через другую функцию из
  класса 'Map', указанную в комментарии. Например, 'fromList'
  требуется выразить через 'insert' (и, возможно, какие-то другие
  стандартные функции).

  Оставшиеся шесть функций считаются минимальной реализацией.

  Обратите внимание, что имена функций @lookup@ и @null@ совпадают
  с определёнными в стандартной библиотеке, поэтому для обращения к ним
  требуется писать @Map.lookup@ и @Map.null@, иначе компилятор не поймёт,
  какую из двух функций вы хотите.

  Строго говоря, 'alter' и 'Map.lookup' можно обобщить до функции
  вроде 'Data.Map.Strict.alterF', которая позволяет при изменении
  'Map' ещё и вытащить наружу старое значение, но мы этим заниматься
  не будем.
-}
class Map t where
    empty :: Ord k => t k a

    singleton :: k -> a -> t k a

    fromList :: Ord k => [(k, a)] -> t k a
    fromList = undefined {- insert -}

    toAscList :: t k a -> [(k, a)]

    insert :: Ord k => k -> a -> t k a -> t k a
    insert = undefined {- insertWith -}

    insertWith :: Ord k => (a -> a -> a) -> k -> a -> t k a -> t k a
    insertWith f k a = alter ( \x -> case x of
            Just value -> Just $ f a value
            Nothing    -> Just a ) k

    insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> t k a -> t k a
    insertWithKey = undefined {- insertWith -}

    delete :: Ord k => k -> t k a -> t k a
    delete = undefined {- alter -}

    adjust :: Ord k => (a -> a) -> k -> t k a -> t k a
    adjust = undefined {- alter -}

    adjustWithKey :: Ord k => (k -> a -> a) -> k -> t k a -> t k a
    adjustWithKey = undefined {- adjust -}

    update :: Ord k => (a -> Maybe a) -> k -> t k a -> t k a
    update = undefined {- alter -}

    updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> t k a -> t k a
    updateWithKey = undefined {- update -}

    alter :: Ord k => (Maybe a -> Maybe a) -> k -> t k a -> t k a

    lookup :: Ord k => k -> t k a -> Maybe a

    member :: Ord k => k -> t k a -> Bool
    member = undefined {- lookup -}

    notMember :: Ord k => k -> t k a -> Bool
    notMember = undefined {- member -}

    null :: t k a -> Bool
    null = undefined {- size -}

    size :: t k a -> Int
