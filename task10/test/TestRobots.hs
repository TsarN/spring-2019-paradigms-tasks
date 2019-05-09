import Test.Tasty
import Test.Tasty.HUnit

import Robots

main :: IO ()
main = defaultMain testsRobots

testsRobots :: TestTree
testsRobots = let
        walter = robot "Walter" 50 100
        mike = robot "Mike" 30 200
        mrdead = robot "Dead" 50 0
    in testGroup "Unit tests for Robots task"
        [ testCase "Test for getName" $
            getName walter @?= "Walter"

        , testCase "Test for getAttack" $
            getAttack walter @?= 50

        , testCase "Test for getHealth" $
            getHealth walter @?= 100

        , testCase "Test for setName" $
            setName "Retlaw" walter @?= robot "Retlaw" 50 100

        , testCase "Test for setAttack" $
            setAttack 42 walter @?= robot "Walter" 42 100

        , testCase "Test for setHealth" $
            setHealth 42 walter @?= robot "Walter" 50 42

        , testCase "Test for printRobot" $
            printRobot walter @?= "Walter, attack: 50, health: 100"

        , testCase "Test for damage" $
            damage walter 25 @?= robot "Walter" 50 75

        , testCase "Test for isAlive on an alive robot" $
            isAlive walter @?= True

        , testCase "Test for isAlive on a dead robot" $
            isAlive mrdead @?= False

        , testCase "Test for fight" $
            fight walter mike @?= robot "Mike" 30 150

        , testCase "Test for threeRoundFight" $
            threeRoundFight walter mike @?= robot "Mike" 30 100

        , testCase "Test for neueRobotAttak" $
            neueRobotAttak mike @?= robot "Mike" 30 (-200)

        , testCase "Test for survivors" $
            survivors @?= [robot "GoldExperienceRequiem" 100 100]
        ]
