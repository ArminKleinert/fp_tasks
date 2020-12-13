{-
Funktionale Programmierung Übung 4
Abgabe von Armin Kleinert und Anna Sophie Pipperr
-}

-- A6

-------------------
-- Addition

-- Makes sure that the size of 2 matrices is valid for addition.
-- It doesn't matter whether the matrix is made up of a list of rows
-- or a list of columns, as long as both matrices follow the same format.
-- (For the documentation, a list of columns is presumed)
-- To make sure that both matrices have not only the same number of columns but 
-- also the same length in every column, an Int 'len' is used. It starts out
-- as -1 and is then set to the length of the first column of the first matrix.
-- 
-- If both matrices have the same number of columns and if all columns have the
-- same size, the function returns True. Otherwise, it returns False.
--
-- checkMatrixSizes [[]] [[]] => True
-- checkMatrixSizes [[1,2],[3,4]] [[4,3],[2,1]] => True
-- checkMatrixSizes [[1,2]] [[4,3],[2,1]] => False (Different column-count)
-- checkMatrixSizes [[1,2],[3,4]] [[4,3]] => False (Different column-count)
-- checkMatrixSizes [[1,2],[3]] [[4,3],[2,1]] => False (Rows with different sizes
checkMatrixSizes :: [[a]] -> [[a]] -> Int -> Bool
checkMatrixSizes [] [] _ = True -- Size is equal
checkMatrixSizes (x:xs) [] _ = False -- First is bigger
checkMatrixSizes [] (x:xs) _ = False -- Second is bigger
checkMatrixSizes (x:xs) (y:ys) len
      | xlen == len1 && len1 == length y = checkMatrixSizes xs ys len1
      | otherwise = False
  where xlen = length x
        len1 = if (len == -1) then xlen else len

-- Performs slow matrix addition. First, the sizes of the matrices are checked 
-- using checkMatrixSizes (see above).
--
-- addMatrices [[0,1],[2,3],[9,1]] [[1,9],[1,1], [9,19]]
--             => [[1,10],[3,4],[18,20]]
-- addMatrices [[0,1],[2,3]] [[1,9],[1,1]]
--             => [[1,10],[3,4]]
-- addMatrices [[0,1],[2,3],[9]] [[1,9],[1,1], [9]]
--             => *** Exception: Matrix sizes must be equal!
addMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
addMatrices m0 m1
    | null m0 || null m1 = error "Matrices must not be empty!"
    | checkMatrixSizes m0 m1 (-1) = zipWith (zipWith (+)) m0 m1
    | otherwise = error "Matrix sizes must be equal!"

-------------------
-- Multiplication

-- FIXME
-- Validates the size of 2 matrices for multiplication. 
-- The Number of columns in matrix 1 must be equal to the number of rows in 
-- matrix 2.
checkMatrixSizes1 :: [[a]] -> [[a]] -> Int -> Int -> Int -> Bool
checkMatrixSizes1 [] [] ccm0 ccm1 rcm0 = ccm1 == rcm0 -- row count m1 == column count m0
checkMatrixSizes1 (x:xs) [] ccm0 ccm1 rcm0 -- m0 is not empty yet
    | length x == _ccm0 = checkMatrixSizes1 xs [] _ccm0 _ccm1 (rcm0 + 1)
    | otherwise = False -- m0 has inconsistent row count
  where _ccm0 = if ccm0 == (-1) then length x else ccm0
        _ccm1 = if ccm1 == (-1) then 0 else ccm1
checkMatrixSizes1 [] (y:ys) ccm0 ccm1 rcm0 -- m1 is not empty yet
    | length y == _ccm1 = checkMatrixSizes1 [] ys _ccm0 _ccm1 rcm0
    | otherwise = False -- m1 has inconsistent row count
  where _ccm0 = if ccm0 == (-1) then 0 else ccm0
        _ccm1 = if ccm1 == (-1) then length y else ccm1
checkMatrixSizes1 (x:xs) (y:ys) ccm0 ccm1 rcm0
    | length x /= _ccm0 = False -- m0 has inconsistent row count
    | length y /= _ccm1 = False -- m1 has inconsistent row count
    | otherwise = checkMatrixSizes1 xs ys _ccm0 _ccm1 (rcm0 + 1)
  where _ccm0 = if ccm0 == (-1) then length x else ccm0
        _ccm1 = if ccm1 == (-1) then length y else ccm1


-- FIXME
mulMatrices :: Num a => [[a]] -> [[a]] -> [[a]]
mulMatrices m0 m1
    | null m0 || null m1 = error "Matrices must not be empty!"
    | checkMatrixSizes1 m0 m1 (-1) (-1) 0 = map (mult [] m1) m0
    | otherwise = error "Invalid matrix sizes for multiplication!"
  where
    mult xs [] _ = xs
    mult xs _ [] = xs
    mult [] (zs:zss) (y:ys) = mult (map (y *) zs) zss ys
    mult xs (zs:zss) (y:ys) = mult (zipWith (\u v -> u + v * y) xs zs) zss ys



test :: IO ()
test = putStrLn ("addMatrices" ++
                 "\n[[0,1],[2,3],[9,1]] [[1,9],[1,1], [9,19]] => " ++ (show (addMatrices [[0,1],[2,3],[9,1]] [[1,9],[1,1], [9,19]])) ++
                 "\n[[0,1],[2,3]] [[1,9],[1,1]] => " ++ (show (addMatrices [[0,1],[2,3]] [[1,9],[1,1]])))
