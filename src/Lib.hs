module Lib where

import Data.List

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPES!
data MaritalStatus = Single | Married | Widowed
                   deriving (Show, Read, Eq)

data Gender = Male | Female
            deriving (Show, Read, Eq)

data AcademicTitle = DiS | Bc | Mgr | Ing | PhDr | MUDr | PhD | Doc | Prof
                   deriving (Show, Read, Ord, Eq, Bounded, Enum)

data Person = Person { pFirstname     :: String
                     , pLastname      :: String
                     , pGender        :: Gender
                     , pMaritalStatus :: MaritalStatus
                     , pAge           :: Int
                     , pATitles       :: [AcademicTitle]
                     }

-- | Full czech salutation (in nominative - i.e. první pád)
-- |
-- | "pan doktor Pavel Novák", "paní inženýrka Karolína Šťastná"
-- | "slečna Petra Králová", "Jan" (kid)
-- | if younger than 15 -> kid -> just firstname
-- | if female younger than 25 without academic title and single -> "slečna"
-- | otherwise "pan/paní" depending on the gender
-- | if academic titles, pick the most important (nothing for DiS and Bc)
-- |
-- | https://www.muni.cz/o-univerzite/uredni-deska/oslovovani-akademickych-pracovniku
-- | http://www.etiketavse.estranky.cz/clanky/etiketa/4.-oslovovani-a-spolecenska-vyznamnost.html
-- | http://www.studenta.cz/vysokoskolske-tituly-jak-oslovovat-na-akademicke-pude/magazin/article/587
-- TODO: implement czech salutation which passes the tests
czechSalutation :: Person -> String
czechSalutation p 
    | (pAge p) < 15 = pFirstname p
    | otherwise = (genderSalut p) ++ (titleStatus p) ++ (nameSalut p)

genderSalut :: Person -> String
genderSalut p
    | (pGender p) == Male = "pan "
    | (pAge p) < 25 && (pMaritalStatus p) == Single && (pATitles p) == [] = "slečna "
    | otherwise = "paní "

titleStatus :: Person -> String
titleStatus p
    | (pATitles p) == [] = ""
    | otherwise = (titleS p)

titleS :: Person -> String
titleS p 
    | Prof == pMaxTitle = "profesor" ++ (titleGender p "ka")
    | Doc == pMaxTitle = "docent" ++ (titleGender p "ka")
    | elem pMaxTitle [PhD, MUDr, PhDr] = "doktor" ++ (titleGender p "ka")
    | Ing == pMaxTitle = "inženýr" ++ (titleGender p "ka")
    | Mgr == pMaxTitle = "magistr" ++ (titleGender p "a")
    | otherwise = ""
    where pMaxTitle = maximum (pATitles p)

titleGender :: Person -> String -> String
titleGender p s
    | (pGender p) == Male = " "
    | otherwise = s ++ " "

nameSalut :: Person -> String
nameSalut p = (pFirstname p) ++ " " ++ (pLastname p)

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPE!
-- https://en.wikipedia.org/wiki/Allen%27s_interval_algebra
-- Notice that even DATA CONSTRUCTOR can be written in infix by using ` `
-- - it is normal, because data constructor is actually function!
--
--                                 X                Y
data AllensIAlgebraRelation a = (a, a) `Equals`   (a, a) -- X = Y
                              | (a, a) `Before`   (a, a) -- X < Y
                              | (a, a) `Meets`    (a, a) -- X m Y
                              | (a, a) `Overlaps` (a, a) -- X o Y
                              | (a, a) `Starts`   (a, a) -- X s Y
                              | (a, a) `During`   (a, a) -- X d Y
                              | (a, a) `Finishes` (a, a) -- X f Y
                             deriving (Show, Read, Eq)

-- | Compare two intervals given as tuples and return appropriate
-- | Allen's Interval Algebra relation between them
-- | It assumes that for (x, y) is always x <= y
-- TODO: implement Allen's algebra relation detection of intervals
allensComparison :: Ord a => (a, a) -> (a, a) -> AllensIAlgebraRelation a
allensComparison = undefined

-------------------------------------------------------------------------------
-- DO NOT EDIT DATA TYPE!
data Shape2D = Circle { ciRadius :: Double }
             | Square { sqSide :: Double }
             | Rectangle { reWidth :: Double, reHeight :: Double }
             | Triangle { triSideA :: Double, triSideB :: Double, triSideC :: Double }
             deriving (Show, Read, Eq)

-- TODO: implement circumference calculation for 2D shapes
shapeCircumference :: Shape2D -> Double
shapeCircumference = undefined

-- TODO: implement area calculation for 2D shapes
shapeArea :: Shape2D -> Double
shapeArea = undefined

-------------------------------------------------------------------------------
-- | Geometric sequence as infinite list
-- | https://en.wikipedia.org/wiki/Geometric_progression
-- TODO: implement geometric series
geometricSequence :: Num b => b -> b -> [b]
geometricSequence a r = undefined


-- TODO: implement infinite list of primes [2, 3, 5, 7, 11, ...]
primes :: [Integer]
primes = undefined

-- TODO: implement list of prime factors for given number (use primes list)
factorization :: Integer -> [Integer]
factorization = undefined


-- | Euler's totient function
-- | https://en.wikipedia.org/wiki/Euler%27s_totient_function
-- TODO: implement phi(n) by using search in primes & factorization
phi :: Integer -> Integer
phi = undefined

-------------------------------------------------------------------------------
-- !!! DO NOT COPY, JUST IMPORT (avoid conflicts, pick the best option for you)
-- iii visit the content of modules
-- TODO: replace undefined with "example1" from Data.DummyList.Examples module
dummyListExample1 = undefined
-- TODO: replace undefined with "example2" from Data.MyString.Examples module
stringExample2 = undefined
-- TODO: replace undefined with "example3" from Data.MyString.Examples module
stringExample3 = undefined
