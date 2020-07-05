-- The issue is also present with regular lazy evaluation, but it seems to occur
-- a bit more frequently when strictly evaluating everything.
{-# LANGUAGE Strict #-}

-- | Minimal example of an instability issue with accelerate-llvm-ptx.
module Main where

import           Data.Array.Accelerate         as A
import           Data.Array.Accelerate.LLVM.PTX
                                               as A
import           Data.List                      ( intercalate
                                                , group
                                                , sort
                                                )
import           Prelude                       as P

matrixSize :: DIM2
matrixSize = Z :. 512 :. 512

-- | A matrix filled with zeroes.
zeroes :: Matrix Float
zeroes = fromFunction matrixSize (const 0.0)

-- | Some value that will be added to the matrix on every iteration.
someValue :: Float
someValue = 0.5

-- | A 512x512 matrix filled with 'someValue'.
someMatrix :: Matrix Float
someMatrix = fromFunction matrixSize (const someValue)

-- | Compose a function @n@ times with itself.
doTimes :: Int -> (a -> a) -> a -> a
doTimes n f = (P.!! n) . P.iterate f

-- | Add two matrices componentwise. We will apply this function a few times in
-- succession and then check the output.
add :: Acc (Matrix Float) -> Acc (Matrix Float) -> Acc (Matrix Float)
add = A.zipWith (+)

main :: IO ()
main = putStrLn "Running until encountering unexpected results..."
  >> runTillError 1
 where
  -- | The number of times to call 'Data.Array.Accelerate.LLVM.PTX.runN' in
  -- succession. This should result in a matrix filled with 'someValue'
  -- multiplied by this number.
  iterationsPerLoop = 30

  -- | After adding 'someMatrix' to an empty matrix 'iterationsPerLoop' times,
  -- the resulting value should of course be equal to 'someMatrix' times
  -- 'iterationsPerLoop' (ignoring any potential floating point rounding errors
  -- of course, but the results should at least be stable).
  expectedValue :: Float
  expectedValue = someValue * P.fromIntegral iterationsPerLoop

  checkValue :: Float -> Bool
  checkValue value = value P.== expectedValue

  runTillError :: Int -> IO ()
  runTillError iter =
    -- TODO: The issue occurs when strictly evaluating 'results' below, verify
    --       whether it also occurs when strictly evaluating @A.toList results@
    --       directly
    let results = (doTimes iterationsPerLoop $ runN add someMatrix) zeroes
    in
      if P.all checkValue (A.toList results)
        then runTillError (succ iter)
        else
          error
          $    "Unexpected results after "
          P.++ show iter
          P.++ '×'
          :    show iterationsPerLoop
          P.++ " iterations.\nExpected: "
          P.++ show (A.arraySize results)
          P.++ '×'
          :    show expectedValue
          P.++ "\nActual:   "
          P.++ intercalate
                 ", "
                 ( P.map (\xs@(x : _) -> show (P.length xs) P.++ '×' : show x)
                 . group
                 . sort
                 . P.filter (P.not . checkValue)
                 $ A.toList results
                 )
