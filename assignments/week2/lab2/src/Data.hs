module Data where
import Prelude(Show, undefined)

-- When thinking about data in Haskell always think

-- What is the only way to create a data?

-- What is the only way to use a data?


data Bool = True | False deriving Show

exampleBool1 = True
exampleBool2 = False

and True True   = True
and True False  = False
and False True  = False
and False False = False


data Nat = Zero | Succ Nat deriving Show

three = Succ(Succ(Succ Zero))
five = Succ(Succ(three))

add Zero Zero         = Zero
add Zero (Suc n)      = Succ n
add (Succ n) Zero     = Succ n
add (Succ n) (Succ m) = (Succ(Succ(Add n m)))

add2 Zero x = x
add2 (Succ m) n  = Succ(add2 m n)

-- now write your own data type, for the colors of a stop light

-- data Stoplight = ... deriving Show

data StopLight = Red | Yellow | Green deriving Show

favoriteColor = Green

leastFavoriteColor = Red five

canGo Green  = True
canGo (Red _)= False
canGo Yellow = True
canGo White  = True

--abbreviation:

canGo2 (Red _) = False
canGo2 _ = True

data ListNat = NilNat | ConsNat Nat ListNat deriving Show

exampleListNat = ConsNat Zero (NilNat)

lengthOfListNat NilNat  = Zero
lengthOfListNat (ConsNat _ x) = Succ (lengthOfListNat x)


data ListBool = NilBool | ConsBool Bool ListBool deriving Show

exampleListBool = ConsBool True (ConsBool False (NilBool))

lengthOfListBool NilBool= Zero
lengthOfListBool (ConsBool _ x)  = Succ (lengthOfListBool x)

-- ... this get's very tiresome

data List a = Nil | Cons a (List a) deriving Show -- where a means anything

exampleList = Cons Zero (Cons five (Nil))

length Nil = Zero

length (Cons _ x) = Zero

-- data StudentYear = ... deriving Show -- where a means anything

data StudentYear = Freshman | Sophomore | Junior | Senior deriving Show 

exampleStudentYear = Freshman

-- write an function that shows how many years a student is expected to graduate.  For instance a Sophomore is expected to graduate in 3 years.

expectedYearsToGraduate Senior    = Succ Zero
expectedYearsToGraduate Junior    = Succ (Succ Zero)
expectedYearsToGraduate Sophomore = Succ (Succ (Succ Zero))
expectedYearsToGraduate Freshman  = Succ (Succ (Succ (Succ Zero)))

data Maybe a = Nothing | Just a deriving Show

-- how can we write the inverse?

fromExpectedYearsToGraduate (Succ Zero)                      = Just Senior
fromExpectedYearsToGraduate (Succ (Succ Zero))               = Just Junior
fromExpectedYearsToGraduate (Succ (Succ (Succ Zero )))       = Just Sophomore
fromExpectedYearsToGraduate (Succ (Succ (Succ (Succ Zero)))) = Just Freshman
fromExpectedYearsToGraduate _ = Nothing



-- what is the smallest datatype you can come up with?
--empty data types
data Smallests = Dot deriving Show

exampleSmallest = Dot



-- what is the crazieset datatype you can come up with?
data Craziests a = Boolean Bool 
				 | Car Stoplight Nat StudentYear
				 | Train Craziests (List a) 
				 | SpeedBoat 
				 deriving Show



exampleCraziests = undefined


