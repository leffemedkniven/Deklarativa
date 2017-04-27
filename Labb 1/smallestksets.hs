
-- Main function. runs the program using argument k(Non-zero positive integer) and a list of Ints.
mainF :: Int -> [Int] -> IO()
mainF k [] = putStrLn("Empty list")
mainF k xs
        | k <= 0 = putStrLn("Integer must be non-zero positive integer")
        -- | k > length xs = putStrLn("There is not that many total subsets.")
        | otherwise = putStrLn("size   i   j   sublist\n" ++ printTuple(smallestksets k xs))

-- tail- and initSubseq makes a list with tuples containing lists of subsequences with corresponding indices.
-- tailSubseq puts the whole list into a tuple and with the help of counters i and j calculates the first
-- and last index of the subsequence, counter i always starts at 1 and for every tailSubseq-call it increments.
-- Then the function does a recursive call with new index i+1 and the list except the first element using tail-function..
-- This then runs until the list is empty, initSubseq works the same way except it uses init-function to recursively call
-- itself with the list except the last element and it decrements the counter j which always starts at total length of the list.
initSubseq :: Int -> Int -> [Int] -> [(Int, Int, [Int])]
initSubseq i j [x] = [(i,j,[x])]
initSubseq i j xs = [(i, j, xs)] ++ initSubseq i (j-1) (init xs)

tailSubseq :: Int -> Int -> [Int] -> [(Int, Int, [Int])]
tailSubseq i j [x] = [(i,j,[x])]
tailSubseq i j xs = ([(i, j, xs)] ++ tailSubseq (i+1) j (tail xs) ++ initSubseq i (j-1) (init xs))

-- first,second and third takes the first, second and third element of a 3-tuple.
first :: (Int, Int, [Int]) -> Int
first (x,_,[]) = x
first (x,_,_) = x

second :: (Int, Int, [Int]) -> Int
second (_,x,[]) = x
second (_,x,_) = x

third :: (Int, Int, [Int]) -> [Int]
third (_,_,[]) = []
third(_,_,x) = x

-- Calculates sum of each subsequence and then puts the sum into a list of quadruples together with indices and list of subsequence.
-- Takes out every part of the 3-tuple and then calculates sum of the list using the sum-function. Puts the result in front of the
-- quadruple then indices and then the subsequence. Then recursively calls with the whole list with everything except the first
-- (already calculated) element. Until the list is empty.
sumSubseq :: [(Int, Int, [Int])] -> [(Int, Int, Int,[Int])]
sumSubseq [x] = [(sum (third x), (first x), (second x), third(x))]
sumSubseq (x : xs) = (sum (third x), (first x), (second x), third(x)) : sumSubseq xs

-- Takes out the sum(first element) of the quadruple for further use.
firstSum :: (Int, Int, Int,[Int]) -> Int
firstSum (x,_,_,[]) = x
firstSum (x,_,_,_) = x

-- Compares sums with eachother. If the sum of x(first element of list) is smaller or equal to y(second element of list)
-- put it before y. If bigger compare x to the third element of the list and so on.
insertSubseq :: (Int,Int, Int,[Int]) -> [(Int,Int, Int,[Int])] -> [(Int,Int, Int,[Int])]
insertSubseq x [] = [x]
insertSubseq x (y : ys)
          | firstSum x <= firstSum y = x : y : ys
          | otherwise = y : insertSubseq x ys

-- Makes a sorted list of subsequences corresponding to sum.
sortSums :: [(Int,Int, Int,[Int])]-> [(Int,Int, Int,[Int])]
sortSums [(x)] = [(x)]
sortSums (x : xs) = insertSubseq x (sortSums xs)

-- Takes the k:th smallest subsequences. Pretty much responsible for starting the whole program. Gives i and j their starting values
-- of 1 and the length of the list.
smallestksets :: Int -> [Int] -> [(Int,Int,Int,[Int])]
smallestksets k [x] = [(x, 1, (length [x]), [x])]
smallestksets k xs = take k (sortSums( sumSubseq (tailSubseq 1 (length xs) xs)))

-- Prints the list of subsequences.
printTuple :: [(Int, Int, Int, [Int])] -> String
printTuple [(summ,i,j,list)] = "  " ++ show summ ++ "   " ++ show i ++ "   " ++  show j ++ "   " ++ show list
printTuple ((summ,i,j,list) : xs) = "  " ++ show summ ++ "   " ++ show i ++
                                          "   " ++  show j ++ "   " ++ show list ++ "\n" ++ printTuple xs
