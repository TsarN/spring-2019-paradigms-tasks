{-# LANGUAGE ScopedTypeVariables #-}  -- Включаем некоторые расширения компилятора.
import Test.Tasty
import Test.Tasty.HUnit
import Data.Proxy
import Map
import qualified Data.Map.Strict as SMap
import MapInstance
import NaiveList(NaiveList)  -- Импортируем только тип NaiveList, но не его конструкторы Nil/Cons, чтобы не путались с конструкторами NaiveTree.
import NaiveTree

main :: IO ()
main = defaultMain testMap

{-|
  Генерирует группу тестов для конкретной реализации 'Map'
  с определённым именем.

  Мы хотим писать тесты один раз для всех возможных реализаций 'Map'.
  В чистом Haskell нам может помочь параметрический полиморфизм,
  но для этого нужно, чтобы в сигнатуре функции присутствовал
  тип из класса 'Map', который мы хотим протестировать.

  Специально для этих целей существует обёртка 'Data.Proxy', он
  позволяет передавать в функции даже типы высшего порядка.
-}
mapTests :: Map m => String -> Proxy m -> TestTree
mapTests name (_ :: Proxy m) =
    -- Чтобы можно было связать типовую переменную m здесь и в let ниже, нужно расширение ScopedTypeVariables.
    testGroup name [
        testGroup "Unit tests - alter and lookup" [
            testCase "insert via alter into an empty map works" $
                let map  = empty :: m Int String in
                let map' = Map.alter (const $ Just "five") 5 map in
                Map.lookup 5 map' @?= Just "five",

            testCase "insert via alter into a singleton map works" $
                let map  = singleton 3 "three" :: m Int String in
                let map' = Map.alter (const $ Just "five") 5 map in
                True @?= ( Map.lookup 3 map' == Just "three" &&
                           Map.lookup 5 map' == Just "five" ),

            testCase "alter into a singleton map works" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.alter (const $ Just "FIVE") 5 map in
                Map.lookup 5 map' @?= Just "FIVE",

            testCase "delete via alter does nothing if element doesn't exist" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.alter (const Nothing) 3 map in
                True @?= ( Map.lookup 3 map' == Nothing &&
                           Map.lookup 5 map' == Just "five" ),

            testCase "delete via alter works" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.alter (const Nothing) 5 map in
                Map.lookup 5 map' @?= Nothing
        ],

        testGroup "Unit tests - insertWith" [
            testCase "insertWith inserts into an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.insertWith (const $ const $ "wrong") 5 "five" map in
                Map.lookup 5 map' @?= Just "five",

            testCase "insertWith alters if value exists" $
                let map = singleton 5 "five" :: m Int String in
                let map' = Map.insertWith (++) 5 "new " map in
                Map.lookup 5 map' @?= Just "new five"
        ],

        testGroup "Various tests" [
            testCase "empty returns an empty map" $
                let map = empty :: m Int String in
                Map.null map @?= True,

            testCase "singleton returns a singleton map" $
                let map = singleton 5 "five" :: m Int String in
                Map.size map @?= 1,

            testCase "toAscList . fromList sorts list" $
                let tr = Map.fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                Map.toAscList tr @?= [(1, "x"), (2, "a"), (3, "c")]
        ]
    ]

testNaiveTree :: TestTree
testNaiveTree = testGroup "Test NaiveTree" [
        testGroup "merge" [
            testCase "merge empty" $
                merge Nil Nil @?= (Nil :: NaiveTree () ())
            ,
            testCase "merge two nodes" $
                -- Ваша реализация может выдавать другое дерево, соответствующее
                -- последовательности 1, 2.
                merge (Node 1 "a" Nil Nil) (Node 2 "b" Nil Nil)
                    @?= Node 1 "a" Nil (Node 2 "b" Nil Nil)
        ]
    ]

testMap :: TestTree
testMap = testGroup "Testing implementations of trees"
    [
--        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList)
--        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
--        testNaiveTree
    ]
