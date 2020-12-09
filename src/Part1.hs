module Part1
  ( prob1
  , prob2
  , prob3
  , prob4
  , prob5
  ) where

import Data.Foldable (find)


------------------------------------------------------------
-- PROBLEM #1
--
-- Реализовать функцию, которая возвращает остаток от
-- деления на 65537 суммы утроенного своего аргумента
-- и числа 123
--
-- На вход функции подаются неотрицательные числа
prob1 :: Int -> Int
prob1 number = (3 * number + 123) `mod` 65537


------------------------------------------------------------
-- PROBLEM #2
--
-- Реализовать функцию, которая:
-- * нечётные числа увеличивает втрое и добавляет единицу
-- * чётные числа делит на два
prob2 :: Integer -> Integer
prob2 number
    | even number = number `div` 2
    | otherwise = number * 3 + 1


------------------------------------------------------------
-- PROBLEM #3
--
-- Реализовать функцию, которая принимает функцию step,
-- положительное число n и пока текущее число не станет
-- равно единице:
-- * вызывает step с текущим числом для получения
--   следующего числа
-- * если текущее число -- единица, возвращает количество
--   выполненных шагов
--
-- Например, если в качестве step используется уменьшение
-- на единицу, а в качестве n передать 5, то должно быть
-- возвращено 4, поскольку последовательность будет такой:
--    5 -> 4 -> 3 -> 2 -> 1
--
-- Если в качестве step передать решение prob2, а n == 3,
-- то ответ 7, а последовательность такая:
--    3 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
--
-- Для любой функции step и n == 1 ответом будет 0.
prob3 :: (Integer -> Integer) -> Integer -> Integer
prob3 stepFunc number = funcWithCounter number 0
    where
        funcWithCounter :: Integer -> Integer -> Integer
        funcWithCounter 1 counter = counter
        funcWithCounter currentNumber counter = funcWithCounter 
            (stepFunc currentNumber)
            (succ counter)

------------------------------------------------------------
-- PROBLEM #4
--
-- Реализовать функцию, возвращающую n-е число Фибоначчи.
-- Нулевое число равно 1, первое тоже 1. Каждое последующее
-- равно сумме двух предыдущих.
--
-- Число n может быть отрицательным, последовательность
-- продолжается естественным образом: (-1)-е число равно 0,
-- далее (-2)-е равно 1, (-3)-е равно (-1), (-4)-е равно 2
-- и т.д. -- сохраняется свойство, что последующие числа
-- равны сумме двух предыдущих.
--
-- Число n по модулю не превосходит 10^5
prob4 :: Integer -> Integer
prob4 seqIndex
    | seqIndex >= 0 = positive 1 1 seqIndex
    | otherwise = negative 1 1 seqIndex
    where
        positive first second currentSeqIndex 
            | currentSeqIndex == 0 = first
            | otherwise = positive second (first + second) (pred currentSeqIndex)

        negative first second currentSeqIndex
            | currentSeqIndex == 0 = second
            | otherwise = negative second (first - second) (succ currentSeqIndex)
    


------------------------------------------------------------
-- PROBLEM #5
--
-- Написать функцию, возвращающую True, если все простые
-- делители первого аргумента n меньше второго аргумента k
--
-- Числа n и k положительны и не превосходят 10^8.
-- Число 1 не считается простым числом
prob5 :: Integer -> Integer -> Bool
prob5 n k = all (< k) (getPrimeDivisors n)
    where
        -- Получить все простые делители числа.
        getPrimeDivisors :: Integer -> [Integer]
        getPrimeDivisors number = getPrimeDivisorsWithCurrentPrime number 2

        getPrimeDivisorsWithCurrentPrime :: Integer -> Integer -> [Integer]
        getPrimeDivisorsWithCurrentPrime inputNumber currentPrime
            | inputNumber `mod` currentPrime == 0
                = currentPrime : getPrimeDivisorsWithCurrentPrime (inputNumber `div` currentPrime) currentPrime
            | inputNumber `elem` primeNumbers = [inputNumber]
            | inputNumber == 1 = []
            | otherwise = getPrimeDivisorsWithCurrentPrime inputNumber $ getNextPrime currentPrime

        -- Получить следующее простое число.
        getNextPrime :: Integer -> Integer
        getNextPrime currentPrime = case find (> currentPrime) primeNumbers of
            Just nextPrime -> nextPrime
            Nothing -> error "input number is too large!"

        -- Список простых чисел.
        primeNumbers :: [Integer]
        primeNumbers = sieve (2 : [3, 5..])
            where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]