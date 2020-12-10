module Part2 where

import Part2.Types

import Data.Function ((&))
import Data.List (nub, find)
import Control.Monad (msum)

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 colorLetter = case colorLetter of
    RED   -> 'R'
    GREEN -> 'G'
    BLUE  -> 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 colorPart = asInt >= 0 && asInt <= 255
    where asInt = prob9 colorPart

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 color colorPart = case colorPart of
    Red   redValue   -> color { red   = (color & red)   + redValue   }
    Green greenValue -> color { green = (color & green) + greenValue }
    Blue  blueValue  -> color { blue  = (color & blue)  + blueValue  }

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 colorPart = case colorPart of
    Red int   -> int
    Green int -> int
    Blue int  -> int

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 color
    | hasDuplicates valuesList = Nothing
    | otherwise = find (\part -> prob9 part == maximum valuesList) colorsList
    where
        colorsList =
            [
                Red   $ color & red,
                Green $ color & green,
                Blue  $ color & blue
            ]
        valuesList = map prob9 colorsList
        hasDuplicates list = length list /= length (nub list)

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
prob11 tree = leftSum + (tree & root) + rightSum
    where
        leftSum = maybe 0 prob11 $ tree & left
        rightSum = maybe 0 prob11 $ tree & right

------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 tree = and
    [
        leftIsSearchTree,
        leftValueIsLess,
        rightValueIsMoreOrEqual,
        rightIsSearchTree
    ]
    where
        leftIsSearchTree  = maybe True prob12 $ tree & left
        rightIsSearchTree = maybe True prob12 $ tree & right

        leftValueIsLess = maybe True
            (\leftSubTree -> (leftSubTree & root) < (tree & root))
            $ tree & left

        rightValueIsMoreOrEqual = maybe True
            (\rightSubTree -> (rightSubTree & root) >= (tree & root))
            $ tree & right

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 value tree
    | value == (tree & root) = Just tree
    | otherwise = msum
        [
            do
                leftSubTree <- tree & left
                prob13 value leftSubTree,
            do
                rightSubTree <- tree & right
                prob13 value rightSubTree
        ]

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 = traverseTree 1
    where
        traverseTree :: Int -> Tree () -> Tree Int
        traverseTree nodeNumber tree = Tree
            (do
                leftSubTree <- tree & left
                return $ traverseTree (succ $ succ nodeNumber) leftSubTree)
            nodeNumber
            (do
                rightSubTree <- tree & right
                return $ traverseTree (succ nodeNumber) rightSubTree)

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 tree = maybe tree leftRotation $ tree & right
    where
        leftRotation rightSubTree = rightSubTree { left = Just oldRoot }
            where
                oldRoot = tree { right = rightSubTree & left }

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 tree = maybe tree rightRotation $ tree & left
    where
        rightRotation leftSubTree = leftSubTree { right = Just oldRoot }
            where
                oldRoot = tree { left = leftSubTree & right }

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
