> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ExistentialQuantification #-}
> {-# LANGUAGE UnicodeSyntax #-}
>
> module KnightsTour where
>
> import Data.Array
> import Data.List ( minimumBy )
> import Parthenopea ( traceIf, traceNever, diagnosticsEnabled )
>   
> visited :: Array Int Bool → (Int, Int) → Bool
> visited bd move = bd ! calcIndex move
>
> noneVisited, allVisited :: Array Int Bool
> noneVisited = listArray (0, numSquares-1) (replicate numSquares False)
> allVisited =  listArray (0, numSquares-1) (replicate numSquares True)
>
> markVisited :: State → (Int, Int) → State
> markVisited (moves, bd) move = (moves', bd')
>   where moves' = move : moves
>         bd' = bd // [(calcIndex move, True)]
>
> calcIndex :: (Int, Int) → Int
> calcIndex (x, y) = x + y*boardXDim
>
> fromIndex :: Int → (Int, Int)
> fromIndex ix = (ix `mod` boardXDim, ix `div` boardYDim)
>
> runAllBruteForce :: IO ()
> runAllBruteForce =
>   do
>     putStrLn "Started runAllBruteForce!"
>
>     let allSolutions = concatMap runEach allSquares
>     print $ length allSolutions
>     if diagnosticsEnabled
>       then print allSolutions
>       else putStrLn " not listed"
>
>     putStrLn "Done runAllBruteForce!"
>  where
>     runEach :: (Int, Int) → [State]
>     runEach initMove = bruteForce (markVisited ([], noneVisited) initMove)
>
> runOneBruteForce :: IO ()
> runOneBruteForce =
>   do
>     putStrLn "Started runOneBruteForce!"
>
>     let initState = markVisited ([], noneVisited) (0, 0)
>     let solutions = bruteForce initState
>     print $ length solutions
>
>     putStrLn "Done runOneBruteForce!"
>
> runSpiralThenBruteForce :: IO ()
> runSpiralThenBruteForce =
>   do
>     putStrLn "Started runSpiralThenBruteForce!"
>
>     let initState = ([], noneVisited)
>     -- print $ tail vals
>     let midState = seedFromSpiral initState vals
>     putStr "length fst midState="
>     print $ length $ fst midState
>     -- print midState
>     let solutions = bruteForce midState
>     let zzstring = if null solutions
>                    then "No solutions"
>                    else show $ length solutions
>
>     putStrLn zzstring
>     putStrLn "Done runSpiralThenBruteForce!"
>
> -- Number the rows and columns from zero to N - 1
> -- Lay a transparent sheet over the NxN chessboard
> -- This measures the squares in terms of Doubles
> -- but has zero in the same place : LL corner
>
> -- The center of the chessboard has location (N/2, N/2)
> boardXDim, boardYDim, numSquares :: Int
> boardXDim = 8
> boardYDim = 8
> numSquares = boardXDim * boardYDim
> allSquares :: [(Int, Int)]
> allSquares = [(x,y) | x ← [0..boardXDim-1], y ← [0..boardYDim-1]]
> center :: (Double, Double)
> center = (fromIntegral boardXDim / 2, fromIntegral boardYDim / 2)
> startRadius = fromIntegral (min boardXDim boardYDim) / 2
>
> type State = ([(Int, Int)], Array Int Bool)
>
> validMoves :: State → (Int, Int) → [(Int, Int)]
> validMoves (bs, bd) (x, y) =
>   if bd == noneVisited
>   then allSquares
>   else filter (validMove bd) (map (addEm (x,y)) allMoves)
>   where
>     addEm :: (Int, Int) → (Int, Int) → (Int, Int)
>     addEm (a,b) (c,d) = (a+c, b+d) 
>     allMoves :: [(Int, Int)]
>     allMoves = [  ( 2, 1), ( 2,-1)
>                 , (-2, 1), (-2,-1)
>                 , ( 1, 2), ( 1,-2)
>                 , (-1, 2), (-1,-2)  ]
>     validMove :: Array Int Bool → (Int, Int) → Bool
>     validMove bd (x,y) = 0 <= x && boardXDim > x && 0 <= y && boardYDim > y && not (visited bd (x,y))
>
> bruteForce :: State → [State]
> bruteForce (bs, bd)
>   | allVisited == bd = [(bs, bd)]
>   | otherwise = concatMap (branch (bs, bd)) moves
>   where 
>     moves = validMoves (bs, bd) (head bs)
>     branch :: State → (Int, Int) → [State]
>     branch (bs, bd) move = bruteForce (bs', bd')
>       where
>         (bs', bd') = markVisited (bs, bd) move
>
> type GradedMove = (Double, (Int, Int))
>
> findBestMove :: (Double, Double, Double, Double) → [(Int, Int)] → (Int, Int)
> findBestMove target moves = snd $ minimumBy sortByGrade $ map (grade target) moves
>   where     
>     grade :: (Double, Double, Double, Double) → (Int, Int) → (Double, (Int, Int))
>     grade (x', y', _, _) (x, y) = (dx * dx + dy * dy, (x, y))
>       where
>         dx, dy :: Double
>         dx = x' - fromIntegral x
>         dy = y' - fromIntegral y
>     sortByGrade :: (Double, (Int, Int)) → (Double, (Int, Int)) → Ordering
>     sortByGrade (grade, _) (grade', _) = compare grade grade'
>
> seedFromSpiral :: State → [(Double, Double, Double, Double)] → State
> seedFromSpiral (bs, bd) (target : xyzws)
>    | traceIf msg False = undefined
>    | otherwise = result
>   where
>     -- if bs is empty, value of startPos is irrelevant
>     startPos = if null bs then (0,0) else head bs
>     moves = validMoves (bs, bd) startPos
>     best =      if null moves
>                 then error "no available move"
>                 else findBestMove target moves
>     (bs', bd') = markVisited (bs, bd) best
>     result =    if null xyzws
>                 then (bs', bd')
>                 else seedFromSpiral (bs', bd') xyzws
>     msg = unwords
>            [ "seedFromSpiral: pos = ",    show $ head bs'
>              , ", target = ",             show target]
>
> traceSpiral :: (Double, Double) → Double → Double → [(Double, Double, Double, Double)]
> traceSpiral origin radius angle
>   | traceNever msg False     = undefined
>   | radius <= 0.5         = []
>   | otherwise             = curPos : traceSpiral origin radius' angle'
>   where
>     curPos = (radius * cos angle + fst origin, radius * sin angle + snd origin, 0, 0)
>     -- 1. the next "pos" should result from moving for a distance of sqrt 3 around the circle
>     -- 2. the radius should decrease by some amount ; consider going around the full circle at the
>     --    (changing) radius so that the resulting "pos" ends up at distance of 1.5
>
>     -- I.E. what incremental angle will move the dot sqrt 3 along the circle
>     slices = 2.40 * pi * radius / sqrt 5
>     angleInc = 2 * pi / slices
>     radiusDec = 1 / slices
>     angle' = angle + angleInc
>     radius' = radius - radiusDec
>     msg = unwords
>       [ " angleInc=",     show angleInc
>        , "radiusDec=",    show radiusDec
>        , "(radius=",      show radius
>        , " nangle=",      show angle,
>          ")" ]
>
> vals, vals' :: [(Double,Double,Double,Double)]
> vals = traceSpiral center (fromIntegral (min boardXDim boardYDim) / 2) 0
>
> vals' = 
>   let (bs, bd) = seedFromSpiral ([], noneVisited) vals
>   in gradeSpiral (bs, bd) vals
>
> gradeSpiral :: State → [(Double, Double, Double, Double)] → [(Double, Double, Double, Double)]
> gradeSpiral (bs, bd) inVals = outVals
>   where outVals = zipWith gradeChoice (reverse bs) inVals
>
> gradeChoice :: (Int, Int) → (Double, Double, Double, Double) → (Double, Double, Double, Double)
> gradeChoice (a, b) (x, y, z, w) = (x, y, z', w')
>   where
>     w' = x - fromIntegral a
>     z' = y - fromIntegral b
>   