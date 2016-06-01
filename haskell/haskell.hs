--Implementation of fst, snd, thd(?) for 3-tuples.
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

--Implementation of head using pattern matching.
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
--error takes a string generates a runtime error with it.
--Note that it causes the program to crash, so use it sparingly.
head' (x:_) = x 
--We use _ since we don't care about the rest of the list.
--We need to use parentheses if we want to bind several variables in 
--a pattern matching.

--A trivial function for demonstrating pattern matching with lists.
tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "This list has one element: " ++ show x
--The binding can also be written as [x]
tell (x:y:[]) = "This list has two elements: " ++ show x ++ " and " ++ show y
--The binding can also be written as [x,y]
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++
  " and " ++ show y

--Implementing length with recursion.
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs
--Here we discard the head because its value is not important.

--Implementing sum with recursion.
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--Patterns work like variables. When we've broken up something into patterns
--and want to bind it to a name so that we can reference the whole thing easily,
--we use patterns. 
--Quick example using patterns.
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--Note that ++ does not work in pattern matches.

--While patterns look for matching input patterns, guards check for 
--boolean conditions.
--The last guard is usually otherwise, which is always True.
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height 
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)
--We can also use pattern matching in where bindings.
--Note that where bindings aren't shared across function bodies 
--of different patterns.

--Another example using guards.
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

--Guards can also be written in-line, albeit resulting in less readability.
--max' :: (Ord a) => a -> a -> a
--max' a b | a > b = a | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
  | a > b = GT 
  | a == b = EQ
  | otherwise = LT 

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ ". "
  where (f:_) = firstname
        (l:_) = lastname

--A version without where bindings.
initials' :: String -> String -> String
initials' (f:_) (l:_) = [f] ++ ". " ++ [l] ++ ". "

--Just as we can define constants in where blocks, we can also define functions.
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2 
  --ie. This is the function definition.

--The pattern for let is: let <bindings> in <expression>.
--The let bindings are accessible only in the in part and not across guards.
cynlinder :: (RealFloat a) => a -> a -> a
cynlinder h r =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea
--let statements are expressions while where statements
--are just syntactic constructs.

--Case expressions work like pattern matching.
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

--This works the same way as the case expression version.
describeList' :: [a] -> String
describeList' [] = "The list is empty."
describeList' (x:[]) = "The list is a singleton list."
describeList' (x:y:_) = "The list is a longer list."

--We can also use case expressions for matching against something
--in the middle of an expression like so, since pattern matching is
--just syntactic sugar for case expressions.
describeList'' :: [a] -> String
describeList'' xs = "The list is " ++ what xs
  where what [] = "empty."
        what [x] = "a singleton list."
        what xs = "a longer list."

--Recursion example.
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = maximum' xs

--Alternative version.
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)
--Notice how we split the head and tail using pattern matching and
--use those parts in the output function.

--Num is not a subclass of Ord, because what constitutes a number doesn't really
--have to adhere to an ordering. When doing subtraction/addition, the Num class
--is necessary.
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x
--We add one x at each recursive call. The last call returns an empty list,
--which is concatenated using :.

--Notice the absence of an otherwise guard, that just means control will fall
--to the next expression in case the first guard doesn't catch it.
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = [] --This condition will terminate the recursion.
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--Note that this is an infinite loop.
repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool 
elem' _ [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = a `elem'` xs

--Notice that head is the pivot here. Running time in the worst case?
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

--Higher-order/curried functions and partial applications.
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

--This can also be rewritten as:
compareWithHundred' :: (Num a, Ord a) => a -> Ordering
compareWithHundred' = compare 100
--compare 100 returns a function that takes a parameter and compares it with 
--100, which was exactly what we wanted in the original function.
--Notice that we only partially applied compare here, which is why it will
--return a function instead of an Ordering.

--Infix functions can also be applied partially by using sections.
--To section an infix function, simply surround it with parentheses and only
--supply a parameter on one side, which creates a function that takes one 
--parameter and applies it only to the side that's missing an operand.
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

--The only special case is with subtraction: use 'subtract' in place of '-'.
--For example, -4 means minus 4 instead of subtract 4.

--Sometimes, if we try to partially apply functions in GHCI instead of binding
--it to a name with a 'let' or passing it to another function, GHCI will throw
--an error because the returned function is not necessarily part of the Show
--typeclass, and thus GHCI can't print/show it like it normally does.

--In this case, this function takes a function and 2 lists as parameters.
--Note that it will also accept a (a -> a -> a) function as well.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x

--But since the functions are curried, we can simplify.
flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f x y = f y x
--We can call flip' f without the parameters and it would return an f that
--takes 2 parameters but calls them fixed.

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

--p for predicate.
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs --If x satisfies p, include x.
  | otherwise = filter' p xs

--A more readable quicksort using filter in place of list comprehensions.
quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
  let smallerSorted = quicksort' (filter (<=x) xs)
      biggerSorted = quicksort' (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

--Produces a Collatz chain.
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

--This function's type is Int because length returns Int instead of Num 
--for historical reasons.
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
  where isLong xs = length xs > 15

--Lambda/anonymous function version.
--Note the absence of a function name. We go straight into pattern matching
--when defining the lambda.
numLongChains' :: Int 
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))
--The only difference with lambdas is that we can't define several patterns
--for one parameter, like making a [] and a (x:xs) pattern for the same 
--parameter and then having the values fall through each pattern.
--If pattern matching fails in a lambda, a runtime error occurs.

--Fold version. foldl means fold left.
sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs
--Notice that we don't need to define x because foldl extracts x from xs
--for us.

--Taking into account currying, we can write an even more succinct version.
sum''' :: (Num a) => [a] -> a
sum''' = foldl (+) 0
--We can omit xs because foldl (+) 0 returns a function that takes a list.

--When dealing with folds, the accumulator value type and the return value type
--are always the same.
elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldl (\acc x -> if x == y then True else acc) False ys

--Right fold's binary function has the current value as the 1st parameter and
--the accumulator as the 2nd. ie (\x acc -> ...).
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs
--Note that : means prepend and is cheaper than ++.

--Note that right fold works on infinite lists while left fold does not, 
--because if you fold on infinite list from some point on the right, you'll
--eventually reach the beginning.

--foldr1 and foldl1 don't need an explicit starting value and assume the 
--first/last element of the list to be the starting value and starts fold with
--the element next to it.
--They cause runtime errors if called with empty lists. So if a function doesn't
--make sense with an empty list, use foldl1 or foldr1.

--Some library functions implemented using folds.
maximum''' :: (Ord a) => [a] -> a
maximum''' = foldr1 (\x acc -> if x > acc then x else acc)

reverse'' :: [a] -> [a]
reverse'' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

head'' :: [a] -> a
head'' = foldl1 (\x _ -> x)

last' :: [a] -> a
last' = foldr1 (\_ x -> x)
--The last 2 functions are obviously better implemented with pattern matching.