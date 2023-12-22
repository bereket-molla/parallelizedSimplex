module MPSParser where

import System.IO
import Simplex

splitSections :: [String] -> ([String], [String], [String])
splitSections lines =
    let (rowsSection, rest1) = break (=="COLUMNS") $ dropWhile (/="ROWS") lines
        (columnsSection, rest2) = break (=="RHS") $ dropWhile (/="COLUMNS") rest1
        (rhsSection, _) = break (`elem` ["BOUNDS", "ENDATA"]) $ dropWhile (/="RHS") rest2

    in (tail rowsSection, tail columnsSection, tail rhsSection)

parseRows :: [String] -> [(String, String)]
parseRows rows = map parseRow $ filter (not . null) rows
  where
    parseRow row = let parts = words row
                   in (parts !! 1, head parts)

parseColumns :: [String] -> [(String, [(String, Double)])]
parseColumns columns = foldl parseColumn [] $ filter (not . null) columns

  where
    parseColumn acc line =

      let parts = words line
          varName = head parts
          constrName = parts !! 1
          value = read (parts !! 2) :: Double
          existing = lookup varName acc
          
      in case existing of
           Just vals -> (varName, (constrName, value) : vals) : filter ((/= varName) . fst) acc
           Nothing   -> (varName, [(constrName, value)]) : acc

parseRHS :: [String] -> [(String, Double)]
parseRHS rhs = map parseRHSLine $ filter (not . null) rhs

  where
    parseRHSLine line =

      let parts = words line
          constrName = parts !! 1
          value = read (parts !! 2) :: Double

      in (constrName, value)

createLinearProgram :: [(String, String)] -> [(String, [(String, Double)])] -> [(String, Double)] -> LinearProgram
createLinearProgram parsedRows parsedColumns parsedRHS = 

    let objective = createObjective parsedColumns
        constraints = map (createConstraint parsedColumns parsedRHS) parsedRows

    in LinearProgram objective constraints

createObjective :: [(String, [(String, Double)])] -> Objective
createObjective parsedColumns = 

    let objCoeffs = concatMap snd $ filter (("N" ==) . fst) parsedColumns
    in Objective (map snd objCoeffs)

createConstraint :: [(String, [(String, Double)])] -> [(String, Double)] -> (String, String) -> Constraint
createConstraint parsedColumns parsedRHS (constrName, constrType) = 
    
    let coeffs = concatMap snd $ filter ((constrName ==) . fst) parsedColumns
        rhs = maybe 0 id $ lookup constrName parsedRHS

    in Constraint (map snd coeffs) rhs

parseMPS :: String -> LinearProgram
parseMPS contents =
   
    let allLines = lines contents
        (rowsSection, columnsSection, rhsSection) = splitSections allLines
        parsedRows = parseRows rowsSection
        parsedColumns = parseColumns columnsSection
        parsedRHS = parseRHS rhsSection

    in createLinearProgram parsedRows parsedColumns parsedRHS

readMPS :: FilePath -> IO LinearProgram
readMPS filePath = do
    
    contents <- readFile filePath
    return $ parseMPS contents
