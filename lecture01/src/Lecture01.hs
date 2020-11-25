module Lecture01 where

-- Nearly all the problems that you see here are from 99 problems in haskell
-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
-- NOTE: Do not copy the solutions from this page IF YOU WANT TO LEARN.
-- Otherwise well go ahead. Even better modify the test suite :P.
--
-- NOTE:
-- 1. Everything in Prelude is fair game to use.
-- 2. Try to solve the problems yourself.
-- 3. Only ask for help from people who attenend the lecture with you.
--    (Will create a channel where we can have descussions on these questions)
-- 4. Don't search for solutions on Google.

-- Problem 1
-- (*) Find the last element of a list.
-- λ> problem1 ['x','y','z']
-- 'z'
problem1 :: [a] -> a
problem1 a = last a


-- Problem 2
-- (*) Find the last but one element of a list.
-- λ> problem2 [1,2,3,4]
-- 3
-- λ> problem2 ['a'..'z']
-- 'y'
problem2 :: [a] -> a
problem2 a = last (init a)


-- Problem 3
-- (*) Find the K'th element of a list. The first element in the list is number 1.
-- Example:
-- λ> problem3 [1,2,3] 2
-- 2
-- λ> problem3 "haskell" 5
-- 'e'
problem3 :: [a] -> Int -> a
problem3 a b = last (take b a)


-- Problem 4
-- (*) Find the number of elements of a list.
-- Example
-- λ> problem4 [123, 456, 789]
-- 3
-- λ> problem4 "Hello, world!"
-- 13
problem4 :: [a] -> Int
problem4 a = sum [1 | x <- a]


-- Problem 5
-- (*) Reverse a list.
-- Example
-- λ> problem5 "A man, a plan, a canal, panama!"
-- "!amanap ,lanac a ,nalp a ,nam A"
-- λ> problem5 [1,2,3,4]
-- [4,3,2,1]
problem5 :: [a] -> [a]
rev a = if null a then [] else last a: rev (init a)
problem5 a = rev a


-- Problem 6
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or
--     backward; e.g. (x a m a x).
-- Example
-- λ> problem6 [1,2,3]
-- False
-- λ> problem6 "madamimadam"
-- True
-- λ> problem6 [1,2,4,8,16,8,4,2,1]
-- True
problem6 :: Eq a => [a] -> Bool
firstLastMatch a = null a || (head a == last a)
problem6 a = firstLastMatch a && firstLastMatch (tail (init a))


-- Problem 7
-- (**) Flatten a nested list structure.
--      Transform a list, possibly holding lists as elements into a `flat' list by
--      replacing each list with its elements (recursively).
-- Example:
-- Example in Haskell:
-- We have to define a new data type, because lists in Haskell are homogeneous.
data NestedList a
  = Elem a
  | List [NestedList a]
-- λ> problem7 (Elem 5)
-- [5]
-- λ> problem7 (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
-- [1,2,3,4,5]
-- λ> problem7 (List [])
-- []

problem7 :: NestedList a -> [a]
problem7 = undefined


-- Problem 8
-- (**) Eliminate consecutive duplicates of list elements.
--      If a list contains repeated elements they should be replaced with a single copy
--      of the element. The order of the elements should not be changed.
-- Example:
-- λ> problem8 "aaaabccaadeeee"
-- "abcade"
problem8 :: Eq a => [a] -> [a]
dropConsec a = if length a == 1 then a 
               else if (head a == head (tail a)) then dropConsec (drop 1 a) 
               else [head a] ++ dropConsec (tail a)
problem8 a = dropConsec a


-- Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains
--      repeated elements they should be placed in separate sublists.
-- Example:
-- λ> problem9 ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
problem9 :: Eq a => [a] -> [[a]]
problem9 = undefined


-- Problem 10
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the
--     so-called run-length encoding data compression method. Consecutive duplicates
--     of elements are encoded as lists (N E) where N is the number of duplicates of
--     the element E.
-- Example:
-- λ> problem10 "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
problem10 :: Eq a => [a] -> [(Int, a)]
problem10 = undefined


-- Problem 11
-- (*) Modified run-length encoding.
--     Modify the result of problem 10 in such a way that if an element has no
--     duplicates it is simply copied into the result list. Only elements with
--     duplicates are transferred as (N E) lists.
--
-- Example in lisp (Where it kinda makes sense):
--
-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- Example in Haskell (where it does not, not so much without IO):
--
-- λ> problem11 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
data ListItem a
  = Single a
  | Multiple Int a
  deriving (Show, Eq)

-- Modified run-length encoding.
problem11 :: Eq a => [a] -> [ListItem a]
problem11 = undefined


-- Problem 12
-- Decode a run-length encoded list.
-- (**) Decode a run-length encoded list.
--      Given a run-length code list generated asspecified in problem 11.
--      Construct its uncompressed version.
--
-- Example:
-- λ> problem12 [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd']
-- "aaaabccaad"
problem12 :: Eq a => [ListItem a] -> [a]
problem12 = undefined


-- Problem 13
-- (**) Run-length encoding of a list (direct solution).
--      Implement the so-called run-length encoding data compression method directly. I.e.
--      don't explicitly create the sublists containing the duplicates, as in problem 9,
--      but only count them. As in problem P11, simplify the result list by replacing the
--      singleton lists (1 X) by X.
--
-- Example:
-- λ> problem13 "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
problem13 :: Eq a => [a] -> [ListItem a]
problem13 = undefined


-- Problem 14
-- (*) Duplicate the elements of a list.
--
-- Example:
-- λ> problem14 [1, 2, 3]
-- [1,1,2,2,3,3]
problem14 :: [a] -> [a]
dupItem a = if length a == 1 then b 
            else b ++ dupItem (tail a) 
            where b = take 2 (repeat (head a))
problem14 a = dupItem a


-- Problem 15
-- (**) Replicate the elements of a list a given number of times.
--
-- Example:
-- λ> problem15 "abc" 3
-- "aaabbbccc"
problem15 :: [a] -> Int -> [a]
replic a = if length a == 1 then b 
           else b ++ replic (tail a) 
           where b = take 3 (repeat (head a))
problem15 a = replic a


-- Problem 16
-- (**) Drop every N'th element from a list.
--
-- Example:
-- λ> problem16 "abcdefghik" 3
-- "abdeghk"
problem16 :: [a] -> Int -> [a]
dropN n a = if length a < n then a else init (take n a) ++ dropN n (drop n a)
problem16 a n = dropN n a


-- Problem 17
-- (*) Split a list into two parts; the length of the first part is given.
--     Do not use any predefined predicates.
--
-- Example:
-- λ> problem17 "abcdefghik" 3
-- ("abc", "defghik")
problem17 :: [a] -> Int -> ([a], [a]) 
problem17 a n = (take n a, drop n a)


-- Problem 18
-- (**) Extract a slice from a list.
--      Given two indices, i and k, the slice is the list containing the elements
--      between the i'th and k'th element of the original list (both limits included).
--      Start counting the elements with 1.
--
-- Example:
-- λ> problem18 ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
problem18 :: [a] -> Int -> Int -> [a]
problem18 a x y = take (y-x+1) (drop (x-1) a)


-- Problem 19
-- (**) Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- Examples:
-- λ> problem19 ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- λ> problem19 ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
problem19 :: [a] -> Int -> [a]
problem19 a n = if n >= 0 then drop n a ++ take n a 
                else drop x a ++ take x a 
                where x = length a - (abs n)


-- Problem 20
-- (*) Remove the K'th element from a list.
--
-- Example:
-- λ> problem20 2 "abcd"
-- ('b',"acd")
problem20 :: Int -> [a] -> (a, [a])
problem20 n xs = [ x | x <- xs, (last (take n xs) /= x) ]


-- Problem A
-- (*) Implement merge:
--     Given two sorted lists give out a sorted list
-- Example:
-- λ> problemA [1,3,4] [2,5,6]
-- [1,2,3,4,5,6]
problemA :: Ord a => [a] -> [a] -> [a]
merge a b = if null a then b 
            else if null b then a 
            else if head a < head b then [head a] ++ merge (tail a) b 
            else [head b] ++ merge a (tail b)
problemA a b = merge a b


-- Problem B
-- (*) Implement merge-sort
--     Given a list returna  sorted list using merge sort algorithm you can use the merge
--     from previous problem
-- Example:
-- λ> problemB [4,3,2,1]
-- [1,2,3,4]
problemB :: Ord a => [a] -> [a]
left a = take cnt a where cnt = length a `div` 2
right a = drop cnt a where cnt = length a `div` 2
mergesort a = if length a ==1 then a else  merge (mergesort (left a)) (mergesort (right a))
problemB a = mergesort a
