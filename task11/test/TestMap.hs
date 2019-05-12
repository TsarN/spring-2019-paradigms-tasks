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
                let map' = Map.insertWith (const $ const "wrong") 5 "five" map in
                Map.lookup 5 map' @?= Just "five",

            testCase "insertWith alters if value exists" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.insertWith (++) 5 "new " map in
                Map.lookup 5 map' @?= Just "new five"
        ],

        testGroup "Unit tests - insert" [
            testCase "insert inserts into an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.insert 5 "five" map in
                Map.lookup 5 map' @?= Just "five",

            testCase "insert replaces value" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.insert 5 "new five" map in
                Map.lookup 5 map' @?= Just "new five"
        ],

        testGroup "Unit tests - insertWithKey" [
            testCase "insertWithKey inserts into an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.insertWithKey (\k new old -> show k ++ new ++ old) 5 "five" map in
                Map.lookup 5 map' @?= Just "five",

            testCase "insertWithKey alters if value exists" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.insertWithKey (\k new old -> show k ++ new ++ old) 5 "new " map in
                Map.lookup 5 map' @?= Just "5new five"
        ],

        testGroup "Unit tests - fromList" [
            testCase "fromList constructs an empty map successfully" $
                let map = fromList [] :: m Int String in
                Map.null map @?= True,

            testCase "fromList constructs a map successfully" $
                let map = Map.fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                True @?= ( Map.size map     == 3        &&
                           Map.lookup 1 map == Just "x" &&
                           Map.lookup 2 map == Just "a" &&
                           Map.lookup 3 map == Just "c" ),

            testCase "toAscList . fromList sorts list" $
                let map = Map.fromList [(2, "a"), (1, "b"), (3, "c"), (1, "x")] :: m Int String in
                Map.toAscList map @?= [(1, "x"), (2, "a"), (3, "c")]
        ],

        testGroup "Unit tests - delete" [
            testCase "delete does nothing on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.delete 5 map in
                Map.null map' @?= True,

            testCase "delete does nothing if key does not exist" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.delete 3 map in
                True @?= ( Map.size map'     == 1           &&
                           Map.lookup 5 map' == Just "five" &&
                           Map.lookup 3 map' == Nothing ),

            testCase "delete deletes the key if it exists" $
                let map  = Map.fromList [(3, "three"), (5, "five")] :: m Int String in
                let map' = Map.delete 3 map in
                True @?= ( Map.size map'     == 1           &&
                           Map.lookup 5 map' == Just "five" &&
                           Map.lookup 3 map' == Nothing )
        ],

        testGroup "Unit tests - adjust" [
            testCase "adjust does nothing on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.adjust ("new " ++) 5 map in
                Map.null map' @?= True,

            testCase "adjust does nothing if key does not exist" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.adjust ("new " ++) 3 map in
                True @?= ( Map.size map'     == 1 &&
                           Map.lookup 5 map' == Just "five"),

            testCase "adjust updates the value if the key exists" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.adjust ("new " ++) 5 map in
                True @?= ( Map.size map'     == 1 &&
                           Map.lookup 5 map' == Just "new five")
        ],

        testGroup "Unit tests - adjustWithKey" [
            testCase "adjustWithKey does nothing on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.adjustWithKey (\k x -> show k ++ "new " ++ x) 5 map in
                Map.null map' @?= True,

            testCase "adjustWithKey does nothing if key does not exist" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.adjustWithKey (\k x -> show k ++ "new " ++ x) 3 map in
                True @?= ( Map.size map'     == 1 &&
                           Map.lookup 5 map' == Just "five"),

            testCase "adjustWithKey updates the value if the key exists" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.adjustWithKey (\k x -> show k ++ "new " ++ x) 5 map in
                True @?= ( Map.size map'     == 1 &&
                           Map.lookup 5 map' == Just "5new five")
        ],

        testGroup "Unit tests - update" [
            testCase "update does nothing on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.update (\x -> if x == "five" then Just "new five" else Nothing) 5 map in
                Map.null map' @?= True,

            testCase "update does nothing if key does not exist" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.update (\x -> if x == "five" then Just "new five" else Nothing) 3 map in
                True @?= ( Map.size map'     == 1 &&
                           Map.lookup 5 map' == Just "five"),

            testCase "update updates the value if the key exists" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.update (\x -> if x == "five" then Just "new five" else Nothing) 5 map in
                True @?= ( Map.size map'     == 1 &&
                           Map.lookup 5 map' == Just "new five"),

            testCase "update deletes the key if function returns Nothing" $
                let map  = singleton 5 "not five" :: m Int String in
                let map' = Map.update (\x -> if x == "five" then Just "new five" else Nothing) 5 map in
                True @?= ( Map.size map'     == 0 &&
                           Map.lookup 5 map' == Nothing)
        ],

        testGroup "Unit tests - updateWithKey" [
            testCase "updateWithKey does nothing on an empty map" $
                let map  = empty :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "five" then Just (show k ++ "new five") else Nothing) 5 map in
                Map.null map' @?= True,

            testCase "updateWithKey does nothing if key does not exist" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "five" then Just (show k ++ "new five") else Nothing) 3 map in
                True @?= ( Map.size map'     == 1 &&
                           Map.lookup 5 map' == Just "five"),

            testCase "updateWithKey updates the value if the key exists" $
                let map  = singleton 5 "five" :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "five" then Just (show k ++ "new five") else Nothing) 5 map in
                True @?= ( Map.size map'     == 1 &&
                           Map.lookup 5 map' == Just "5new five"),

            testCase "updateWithKey deletes the key if function returns Nothing" $
                let map  = singleton 5 "not five" :: m Int String in
                let map' = Map.updateWithKey (\k x -> if x == "five" then Just (show k ++ "new five") else Nothing) 5 map in
                True @?= ( Map.size map'     == 0 &&
                           Map.lookup 5 map' == Nothing)
        ],

        testGroup "Unit tests - member" [
            testCase "member returns False on an empty map" $
                let map = empty :: m Int String in
                Map.member 5 map @?= False,

            testCase "member returns False if key does not exist" $
                let map  = singleton 5 "five" :: m Int String in
                Map.member 3 map @?= False,

            testCase "member returns True if key exists" $
                let map  = singleton 5 "five" :: m Int String in
                Map.member 5 map @?= True
        ],

        testGroup "Unit tests - notMember" [
            testCase "notMember returns True on an empty map" $
                let map = empty :: m Int String in
                Map.notMember 5 map @?= True,

            testCase "notMember returns True if key does not exist" $
                let map  = singleton 5 "five" :: m Int String in
                Map.notMember 3 map @?= True,

            testCase "notMember returns False if key exists" $
                let map  = singleton 5 "five" :: m Int String in
                Map.notMember 5 map @?= False
        ],

        testGroup "Unit tests - helper functions" [
            testCase "empty returns an empty map" $
                let map = empty :: m Int String in
                Map.null map @?= True,

            testCase "singleton returns a singleton map" $
                let map = singleton 5 "five" :: m Int String in
                Map.size map @?= 1
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
        mapTests "Data.Map.Strict" (Proxy :: Proxy SMap.Map),
        mapTests "NaiveList" (Proxy :: Proxy NaiveList)
--        mapTests "NaiveTree" (Proxy :: Proxy NaiveTree),
--        testNaiveTree
    ]
