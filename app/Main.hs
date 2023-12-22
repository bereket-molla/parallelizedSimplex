import Simplex
import MPSParser

printTableau :: Tableau -> IO ()
printTableau = mapM_ (putStrLn . unwords . map show)


parseMPSFile :: FilePath -> IO LinearProgram
parseMPSFile filePath = do
    mpsData <- readFile filePath
    let linearProgram = parseMPS mpsData 
    return linearProgram

main :: IO ()
main = do
    lp <- parseMPSFile "13_13_20_1.mps"

    let tableau = initializeTableau lp
    -- putStrLn "initialized tableau:"
    -- printTableau tableau

    let maybeFinalTableau = simplexAlgorithm tableau
    case maybeFinalTableau of
        Just finalTableau -> do
            putStrLn "Optimal solution found:"
            let solution = extractSolution finalTableau
            print solution
        Nothing -> putStrLn "No solution found or problem is unbounded."
