import Simplex


testObjective = Objective [20, 10]
testConstraints = [Constraint [1, 1] 40,  Constraint [4, 1] 100]
testLP = LinearProgram testObjective testConstraints

printTableau :: Tableau -> IO ()
printTableau = mapM_ (putStrLn . unwords . map show)

testInitializeTableau :: LinearProgram -> IO Tableau
testInitializeTableau lp = do
    let tableau = initializeTableau lp
    putStrLn "initialized tableau:"
    printTableau tableau
    return tableau

testFindPivotColumn :: Tableau -> IO (Maybe Int)
testFindPivotColumn tableau = do
    let pivotCol = findPivotColumn tableau
    putStrLn $ "pivot column: " ++ maybe "none" show pivotCol
    return pivotCol


testFindPivotRow :: Tableau -> Int -> IO (Maybe Int)
testFindPivotRow tableau pivotCol = do
    let pivotRow = findPivotRow tableau pivotCol
    putStrLn $ "pivot Row: " ++ maybe "none" show pivotRow
    return pivotRow

testPerformPivot :: Tableau -> Int -> Int -> IO ()
testPerformPivot tableau pivotRow pivotCol = do
    let newTableau = performPivot tableau pivotRow pivotCol
    putStrLn "tableau after pivot operation:"
    printTableau newTableau


testSimplexAlgorithm :: Tableau -> IO ()
testSimplexAlgorithm tableau = do
    putStrLn "testing simplex algorithm..."
    case simplexAlgorithm tableau of
        Just finalTableau -> do
            putStrLn "final tableau (optimal solution):"
            printTableau finalTableau
        Nothing -> putStrLn "problem is unbounded."


main :: IO ()
main = do
    let tableau = initializeTableau testLP
    putStrLn "initialized tableau:"
    printTableau tableau

    case findPivotColumn tableau of
        Just pivotCol -> do
            putStrLn $ "pivot column: " ++ show pivotCol

            case findPivotRow tableau pivotCol of
                Just pivotRow -> do
                    putStrLn $ "pivot row: " ++ show pivotRow

                    let newTableau = performPivot tableau pivotRow pivotCol
                    putStrLn "tableau after pivot operation:"
                    printTableau newTableau
                Nothing -> putStrLn "no valid pivot row."
        Nothing -> putStrLn "no valid pivot column."
    testSimplexAlgorithm tableau

