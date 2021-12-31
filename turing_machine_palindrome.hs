{-
This script implements a Turing machine that operates on the input and output symbols:
{a, b} and computes a function f(x), where f(x) outputs only the symbols a if x is a palindrome
and outputs the symbol b otherwise. 

-}

-- addToEnd - is a function that adds an element at the end of a list
addToEnd :: [a] -> a -> [a]
addToEnd [] a = [a]
addToEnd (x:xs) a = x:addToEnd xs a 

-- The tape data type, represents an infinite tape of the Turing Machine
data Tape a = Tape [a] a [a] deriving (show, Eq)

-- The state data type represetns the set of states in the Turing Machine
data state = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | QAccept | QReject deriving Eq

-- moveRight and moveLeft - are functions that move the head of the turing machine
-- left or right depending on the input
moveRight, moveLeft :: Tape a -> Tape a
moveRight (Tape xs i (y:ys)) = Tape (addToEnd xs i) y ys
moveLeft (Tape xs i ys) = Tape (init xs) (last xs) ([i]++ys)

-- writeHead - is a function that writes a value at the current position on the tape where the head is
writeHead :: Tape a -> a -> Tape a
writeHead (Tape xs i ys) i' = Tape xs i' ys

-- The TM data type represents the Turing Machine where s is the initial state, Tape a is the tape of the 
-- Turing Machine. Note: Not everythign is included, for simplicity
data TM s a = TM s (Tape a)

-- transFunction - is the transition function of the Turing Machine
transFunction :: State -> Tape Char -> Maybe (State, Tape Char)
transFunction s t@(Tape _ a _) = case (s, a) of 
   (Q0, '_') -> Just (QAccept, writeHead t 'a')
   (Q0, ' ') -> Just (QAccept, writeHead t 'a')
   (Q0, 'a') -> Just (Q1, moveRight(writeHead t '_'))
   (Q1, 'a') -> Just (Q1, moveRight t)
   (Q1, 'b') -> Just (Q1, moveRight t)
   (Q1, '_') -> Just (Q2, moveLeft t)
   (Q2, 'a') -> Just (Q3, moveLeft(writeHead t '_'))
   (Q2, '_') -> Just (Q2, writeHead t '_')
   (Q2, 'b') -> Just (QReject, writeHead t 'b')
   (Q3, 'a') -> Just (Q3, moveLeft t)
   (Q3, 'b') -> Just (Q3, moveLeft t)
   (Q3, '_') -> Just (QAccept, moveRight(writeHead t '_'))
   (Q0, 'b') -> Just (Q4, moveRight(writeHead t '_'))
   (Q4, 'b') -> Just (Q4, moveRight t)
   (Q4, 'a') -> Just (Q4, moveRight t)
   (Q4, '_') -> Just (Q5, moveLeft t)
   (Q5, 'b') -> Just (Q3, moveLeft(writeHead t '_'))
   (Q5, '_') -> Just (QAccept, writeHead t '_')
   (Q5, 'a') -> Just (QReject, writeHead t 'b')
   (QAccept, _) -> Just (QAccept, writeHead t 'a')
   (QReject, _) -> Just (QReject, writeHead t 'b')

-- runTM - is a function that runs the Turing Machine on a paritcular string and outputs a if the
-- string is accepted, and b otherwise.
runTM :: TM State Char -> String
runTM (TM s t) = case transFunction s t of 
     Just (QAccept, _) -> "a"
     Just (QReject, _) -> "b"
     Just (s', t') -> runTM (TM s' t') 

blanks = '_'

-- createTape - is a function that simulates the Turing Machine Tape
createTape :: [Char] -> Tape Char
createTape input_string = Tape [blanks] (head (input_string)) ((drop 1 input_string) ++ [blanks])

-- outputRes - is a function that outputs if a given string is palindrome or not i.e. outputs a or b 
outputRes :: [Char] -> String
outputRes input_string = runTM(TM Q0 (createTape input_string))