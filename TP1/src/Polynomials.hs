module Polynomials where

import Data.Char(digitToInt, isDigit, isAlpha)
import Data.List ( sort, sortBy, groupBy )
import Text.Printf (IsChar(toChar))

type Coefficient = Int
type Literals = [Char]
type Powers = [(Char, Int)]
type Term = (Coefficient, Powers)
type Polynomial = [Term]

--Checks if a String is made up of only digits or only digits and a negative sign on the left
isNumber :: String -> Bool
isNumber ('-':number) = isNumber number
isNumber xs  =
  case dropWhile isDigit xs of
    ""       -> True
    _        -> False

--Checks if a Char is the multiplication or exponent operator
isOperator :: Char -> Bool
isOperator x = x == '*' || x == '^'

--Checks if a Char should be removed from the Polynomial's representation
isDroppableChar :: Char-> Bool
isDroppableChar x = x == '+' || x == '(' || x == ')' || x == ' '

--Splits a Polynomial in its Terms, separating Terms by spaces or plus signs
splitPolynomial :: String -> [String]
splitPolynomial s = case dropWhile isDroppableChar s of
                      "" -> []
                      ('-' : negativeTerm) -> ('-' : head splitNegativeTerm) : tail splitNegativeTerm
                            where splitNegativeTerm = splitPolynomial negativeTerm
                      s' -> w : splitPolynomial s''
                            where (w, s'') = break isDroppableChar s'


--Splits Term (String) in a list of strings. Each string represents a coefficient, a lietral or an exponent
splitTerm :: String -> [String]
splitTerm x = filter (\c -> c /= "*" && c /= "^") (splitTermAux x)

splitTermAux :: String -> [String]
splitTermAux "" = [] 
splitTermAux [x] = [[x]]
splitTermAux ('-':negativeTerm) = ('-':head aux) : tail aux where aux = splitTermAux negativeTerm
splitTermAux x = if takeWhile isDigit x /= "" then takeWhile isDigit x : splitTermAux  (dropWhile isDigit x) -- in case the string starts with a number
                  else [head x] : splitTermAux  (tail x) -- in case there is not a number at the start



stringToChar :: String -> Char
stringToChar [c] = c
stringToChar str = 'M' -- debug

-- States that occur when we are parsing the split terms 
data State =  First | Coefficient | Literal | Exponent  deriving (Eq, Show)
stateChanger :: State -> String -> State
stateChanger state str = case state of
       First | isNumber str -> Coefficient -- in the First state, any number find must be the coefficient
             | otherwise -> Literal        -- otherwise it must represent a literal
       Coefficient | isAlpha (stringToChar str) -> Literal --after the coefficient, if there is any character it must be a letter
                   | otherwise -> error "bad format"
       Literal | isNumber str -> Exponent -- after a literal any number represents an exponent since there is only one coefficient per term
               | otherwise -> Literal
       Exponent | isNumber str -> error "bad format" --after an exponent, if there are remaining characters they must represent a literal
                | otherwise -> Literal

-- Parse a split term by iterating the list of strings and updating the state
parseTerm :: [String] -> State -> Term
--Base cases
parseTerm [] _ = (0,[])
parseTerm [str] state  | isNumber str && state == First = (read str, [])
                       | head str == '-' && state == First = (-1, [(last str, 1)])
                       | head str == '-' && state == Literal = (-1, [(last str, 1)])
                       | state == First = (1, [(stringToChar str, 1)])
                       | state == Literal = (1, [(stringToChar str, 1)])
                       | otherwise = error "parseTerm::error"

parseTerm (x:xs) state =
      case stateChanger state x of -- get the state we will be moving to and interpret the next string accordingly
      Coefficient -> (read x, snd (parseTerm xs (stateChanger Coefficient (head xs)))) -- set the coefficient of the term and get the powers recursively
      Literal -> (if head x == '-' then -1 else 1, if stateChanger Literal (head xs) == Exponent -- verify if we will be reading the exponent next or another literal
                        then (last x, read (head xs)):snd (parseTerm (tail xs) (stateChanger Literal (head (tail xs)))) -- use head xs as the exponent and continue iterating from there
                     else (last x, 1):snd (parseTerm xs (stateChanger Literal (head xs)))) --if we have no explicit exponent it 1 by default
      --Exponent ->  -- will not occur
      --First ->   -- will not occur

-- Parse a Polynomial given as a String
parsePolynomial :: String -> Polynomial
parsePolynomial p = [parseTerm (splitTerm x) First | x<-splitPolynomial p]

-- Verify if a Term is valid, that is, if the coefficient is different from 0
validTerm :: Term -> Bool
validTerm (c,powers) = c /= 0

-- Verify if a Power (symbo, exponent) is valid, that is, if the exponenet is not 0
validPower :: (Char, Int) -> Bool
validPower (c, e) = e /= 0

-- Remove invalid powers from a Term
trimPowers :: Term -> Term
trimPowers (c, powers) = (c, filter validPower powers)

-- Sort a Term by its literals and trim invalid powers
sortTermByLiterals :: Term -> Term
sortTermByLiterals (c, powers) = trimPowers (c, sort powers)

-- Sort a Term by bigger powers first
sortTermByPowers :: Term -> Term
sortTermByPowers t = (fst t, sortBy comparePowers (snd (sortTermByLiterals t)))

-- Auxiliary function for ordering Powers
comparePowers :: (Ord a1, Ord a2) => (a2, a1) -> (a2, a1) -> Ordering
comparePowers (l1, e1) (l2, e2) | e1 > e2 = LT -- bigger exponenets appear first
                                | e1 == e2 && l1 <= l2 = LT -- literals are organized by lexicographical order
                                |otherwise = GT

-- Auxiliary funtion for ordering Terms
compareTerms :: (Coefficient, Powers) -> (Coefficient, Powers) -> Ordering
compareTerms (c1,[]) (c2, powers2) = GT -- terms without literals appear last
compareTerms (c1, powers1) (c2, []) = LT 
compareTerms (c1, powers1) (c2, powers2) | maximum (map snd powers1) > maximum (map snd powers2) = LT -- order by bigger powers first
                                         | maximum (map snd powers1) == maximum (map snd powers2) && powers1 < powers2 = LT --order by the list of symbols
                                         | otherwise = GT

-- Sort a Polynomial's Terms and remove invalid Terms and Powers
sortPolynomial :: Polynomial -> Polynomial
sortPolynomial p = sortBy compareTerms termsSorted
                    where termsSorted = [sortTermByPowers term | term<-joinedPowers]
                          joinedPowers = [ (fst t, joinPowers (groupBy (\x y-> fst x == fst y) (snd t))) | t<-termsSortedByLiterals] -- x^2 * x^3 shall appear as x^5 
                          termsSortedByLiterals = [sortTermByLiterals term | term<-coefficientsSorted]
                          coefficientsSorted = sort p

-- Sums two Terms with equal list of Powers, otherwise returns a specific invalid Term
sumEqualTerms :: Term -> Term -> Term
sumEqualTerms (c1, powers1) (c2, powers2) | sort powers1  == sort powers2 = (c1 + c2,  powers1)
                                   | otherwise = (0,[('_', -1)])

-- Sum Terms with equal Powers in a sorted polynomial
sumTerms :: Polynomial -> Polynomial
sumTerms [] = []
sumTerms [x] = [x]
sumTerms (t1:t2:ts) = let aux = sumEqualTerms t1 t2 in if aux == (0,[('_', -1)]) then t1:sumTerms (t2:ts)
                                                       else sumTerms (aux:ts)

-- Receive a String representing a Polynomial and return a String representing the normalized Polynomial
normalize :: String -> String
normalize polynomial =  convertPolynomialToString (normalize' (parsePolynomial polynomial)) --meter função q transforma em string

-- Normalize a Polynomial by joining equal Terms, removing invalid Terms and sorting the result by higher exponents first
normalize' :: Polynomial -> Polynomial
normalize' p = filter validTerm joinedPowers
                where joinedPowers = [ (fst t, joinPowers (groupBy (\x y-> fst x == fst y) (snd t))) | t<-summed]
                      summed =  sumTerms reduced
                      reduced = sortPolynomial p
                      

-- Add two Polynomials and normalize the result
addPolynomials' :: Polynomial -> Polynomial -> Polynomial
addPolynomials' p1 p2 = normalize' (p1 ++ p2)

-- Add two Polynomials received as String and output the result as a String
addPolynomials :: String -> String -> String
addPolynomials p1 p2 = convertPolynomialToString (addPolynomials' (parsePolynomial p1) (parsePolynomial p2))

-- Given a list of grouped Powers by Literal that are being multiplied, sum the exponents in each group 
joinPowers :: [[(Char, Int)]] -> [(Char, Int)]
joinPowers [] = []
joinPowers groups = [if length g == 1 then head g else (fst (head g), sum (map snd g)) | g<-groups]

-- Multiply two Terms
multiplyTerms :: Term -> Term -> Term
multiplyTerms t1 t2 | map fst powers1 == map fst powers2 = (c1 * c2, [(literals !! n, exponents1 !! n + exponents2 !! n) | n<-[0.. length powers1 - 1]]) -- if the literals of both Terms are equal, multiply Coefficients and sum Exponents
                    | otherwise = (c1 * c2,  joinPowers (groupBy (\x y-> fst x == fst y) (sort (powers1 ++ powers2))) ) --join powers, sort them, group them by symbol and sum the exponents if required
                                                                                                      where literals = map fst powers1
                                                                                                            exponents1 = map snd powers1
                                                                                                            exponents2 = map snd powers2
                                                                                                            (c1, powers1) = sortTermByLiterals t1
                                                                                                            (c2, powers2) = sortTermByLiterals t2
-- Multiply two Polynomials
multiplyPolynomials' :: Polynomial -> Polynomial -> Polynomial
multiplyPolynomials' p1 p2 = normalize' ([multiplyTerms t1 t2 |  t1<-p1, t2<-p2])

-- Multiply two Polynomials given as Strings and return the result as a String
multiplyPolynomials :: String -> String -> String
multiplyPolynomials p1 p2 = convertPolynomialToString (multiplyPolynomials'(parsePolynomial p1) (parsePolynomial p2))

-- Gets the first element in a tuple of 3 elements
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-- Gets the second element in a tuple of 3 elements
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

-- Gets the third element in a tuple of 3 elements
thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

-- Checks if a variable is in the Powers list
varInPowers :: Powers -> Char -> Bool
varInPowers [] var = False
varInPowers (x:xs) var = fst x == var || varInPowers xs var

-- Differentiates a (variable, exponent) tuple in internal form, according to a given variable
differentiateVariables :: Powers -> Char -> [(Int, Char, Int)]
differentiateVariables [] var = []
differentiateVariables (x:xs) var = if fst x == var then (snd x, fst x, snd x - 1) : differentiateVariables xs var else (1, fst x, snd x) : differentiateVariables xs var

-- Differentiates a Term in internal form, according to a given variable
differentiateTerm :: Term -> Char -> Term
differentiateTerm (x, []) var = (0, [])
differentiateTerm (y, l) var | varInPowers l var =  (y * product [fst3 x | x <- differentiateVariables l var], [(snd3 x, thd3 x) | x <- differentiateVariables l var, thd3 x /= 0])
                             | otherwise = (0,[])


-- Differentiates a Polynomial in internal form, according to a given variable
differentiatePolynomial' :: Polynomial -> Char -> Polynomial
differentiatePolynomial' xs var = filter validTerm (map (`differentiateTerm` var) xs)

-- Differentiates a Polynomial in string form, according to a given variable
differentiatePolynomial :: String -> Char -> String
differentiatePolynomial sp var = convertPolynomialToString (normalize' (differentiatePolynomial' (parsePolynomial sp) var))

-- Converts a (variable, exponent) tuple in internal form to a String
convertVariablesToString :: Powers -> String
convertVariablesToString [] = ""
convertVariablesToString (x:xs) | null xs = if snd x == 1 then [fst x] else [fst x] ++ "^" ++ show (snd x)
                                | otherwise = [fst x] ++ (if snd x == 1 then "" else "^" ++ show (snd x)) ++ "*" ++ convertVariablesToString xs

-- Converts a Term in internal form to a String
convertTermToString :: Term -> String
convertTermToString (x, []) = show (abs x)
convertTermToString (y, l) | abs y == 1 = convertVariablesToString l
                           | otherwise = show (abs y) ++ "*" ++ convertVariablesToString l

-- Converts a Polynomial in internal form to a String
convertPolynomialToString :: Polynomial -> String
convertPolynomialToString [] = ""
convertPolynomialToString (x:xs) = if null xs then convertTermToString x else convertTermToString x ++ (if fst (head xs) < 0 then " - " else  " + ") ++ convertPolynomialToString xs

