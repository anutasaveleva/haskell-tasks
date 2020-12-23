module Part2 where

import Part2.Types


------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 RED = 'R'
prob6 GREEN = 'G'
prob6 BLUE = 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 color = 0 <= prob9 color && 255 >= prob9 color

{-
component :: ColorPart -> Int
component (Red x) = x
component (Green x) = x
component (Blue x) = x
-}
------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 color cp = Color {
  red = red color + component2 cp RED,
  green = green color + component2 cp GREEN,
  blue = blue color + component2 cp BLUE
}

component2 :: ColorPart -> ColorLetter -> Int
component2 (Red n) RED = n
component2 (Green n) GREEN = n
component2 (Blue n) BLUE = n
component2 _ _ = 0
------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 (Red x) = x
prob9 (Green x) = x
prob9 (Blue x) = x

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 c
    |length mcount == 1 = Just m
    |otherwise = Nothing
    where
        parts = [Red (red c), Green (green c), Blue (blue c)]
        m = maximum parts
        mcount = filter (\x -> compare x m == EQ) parts
instance Ord ColorPart where
  compare x y = compare (prob9 x) (prob9 y)
------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева

prob11 :: Num a => Tree a -> a
prob11 tree = root tree + sumof (left tree) + sumof (right tree)
  where 
    sumof t = maybe 0 prob11 t
------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
prob12 :: Ord a => Tree a -> Bool
prob12 (Tree Nothing _ Nothing) = True
prob12 (Tree (Just l) r Nothing) = all (<r) (summa l) && prob12 l
prob12 (Tree Nothing r (Just right)) = all (>= r) (summa right) && prob12 right
prob12 (Tree (Just l) r (Just right)) = 
    prob12 (Tree (Just l) r Nothing) && prob12 (Tree Nothing r (Just right))
summa :: Tree a -> [a]
summa (Tree Nothing r Nothing) = [r]
summa (Tree (Just l) r Nothing) = summa l ++ [r]
summa (Tree Nothing r (Just right)) = r : summa right 
summa (Tree (Just l) r (Just right)) = summa l ++ r : summa right
------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 n tree = p13 n (Just tree)
  where
    p13 :: Ord a => a -> Maybe (Tree a) -> Maybe (Tree a)
    p13 _ Nothing = Nothing
    p13 n (Just (Tree l root r))
      | n == root = (Just (Tree l root r))
      | n < root = p13 n l
      | n > root = p13 n r  
------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 = error "Implement me!"

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 (Tree l ro (Just (Tree ri1 ri2 ri3))) = Tree (Just $ Tree l ro ri1) ri2 ri3

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 (Tree (Just (Tree a b c)) d e) = Tree a b (Just (Tree c d e))
------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"
