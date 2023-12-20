module Simplex where 

import Data.List (minimumBy)
import Data.Ord (comparing)


{-

 This implementation assumes that the problem is in standard form:

 1) All constraints are linear inequalities of the form: a^T * x <= b,
   where a and x are vectors, and b is a scalar. Each constraint represents
   one row of the matrix equation A * x <= b.

 2) All variable values (elements of vector x) are non-negative.

 3) The objective functions is to be maximized. 
 
-}

type Coefficients = [Double]
type Bound = Double

data Constraint = Constraint Coefficients Bound 
data Objective = Objective [Double]
data LinearProgram = LinearProgram Objective [Constraint]

type Tableau = [[Double]]

initializeTableau :: LinearProgram -> Tableau
initializeTableau (LinearProgram (Objective objCoeffs) constraints) = 
    let nVars = length objCoeffs
        nConstraints = length constraints

        -- identity matrix of size nConstraints for the slack variables
        identityMatrix = [ replicate i 0 ++ [1] ++ replicate (nConstraints - i - 1) 0 | i <- [0..nConstraints-1]]

        -- combine each constraint with its corresponding slack variable row from the identity matrix
        constraintRows = zipWith (\(Constraint coeffs b) slackRow -> coeffs ++ slackRow ++ [b]) constraints identityMatrix

        objRow = map negate objCoeffs ++ replicate nConstraints 0 ++ [0] -- assuming maximiztion here

    in constraintRows ++ [objRow]


findPivotColumn :: Tableau -> Maybe Int
findPivotColumn tableau = 
    let lastRow = last tableau
        objectiveCoeffs = init lastRow -- exclude the rhs

    in if all (>= 0) objectiveCoeffs
        then Nothing -- no negative coefficients means optimal solution is found
       else Just $ fst $ minimumBy (comparing snd) $ zip [0..] objectiveCoeffs


findPivotRow :: Tableau -> Int -> Maybe Int
findPivotRow tableau pivotCol = 
    let ratios = [ rhs / coeff | row <- init tableau, let rhs = last row, let coeff = row !! pivotCol, coeff > 0 ]

    in if null ratios
       then Nothing -- it's an unbounded problem
       else Just $ fst $ minimumBy (comparing snd) $ zip [0..] ratios


performPivot :: Tableau -> Int -> Int -> Tableau
performPivot tableau pivotRow pivotCol = 
    let pivotElem = (tableau !! pivotRow) !! pivotCol
        newRow = map (/ pivotElem) (tableau !! pivotRow)
    
        newTableau = zipWith (\i row -> if i == pivotRow
                                        then newRow
                                        else let factor = row !! pivotCol
                                             in zipWith (\a b -> a - factor * b) row newRow) [0..] tableau
    in newTableau


simplexAlgorithm :: Tableau -> Maybe Tableau
simplexAlgorithm tableau =
    case findPivotColumn tableau of
         Nothing -> Just tableau -- optimal solution found
         Just pivotCol -> case findPivotRow tableau pivotCol of
                               Nothing -> Nothing -- unbounded
                               Just pivotRow -> simplexAlgorithm $ performPivot tableau pivotRow pivotCol
