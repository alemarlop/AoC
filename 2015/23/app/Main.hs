module Main where
import Data.List.Split (splitOn)

data Instruction = HLF Char | TPL Char | INC Char | JMP Int | JIE Char Int | JIO Char Int deriving (Show)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let lines = map (parseInstruction . words) (splitOn "\n" input)
    print (snd $ executeInstructionAtIndex lines 0 (0, 0), snd $ executeInstructionAtIndex lines 0 (1, 0))

parseInstruction :: [String] -> Instruction
parseInstruction ["hlf", [c]] = HLF c
parseInstruction ["tpl", [c]] = TPL c
parseInstruction ["inc", [c]] = INC c
parseInstruction ["jmp", sign:n] = JMP (if sign == '+' then read n else read n * (-1))
parseInstruction ["jie", c:_, sign:n] = JIE c (if sign == '+' then read n else read n * (-1))
parseInstruction ["jio", c:_, sign:n] = JIO c (if sign == '+' then read n else read n * (-1))

executeInstructionAtIndex :: [Instruction] -> Int -> (Int, Int) -> (Int, Int)
executeInstructionAtIndex instructions index (a, b) =
    if index >= length instructions then (a, b)
    else
        case instructions !! index of
            HLF c -> executeInstructionAtIndex instructions (index + 1) (if c == 'a' then a `div` 2 else a, if c == 'b' then b `div` 2 else b)
            TPL c -> executeInstructionAtIndex instructions (index + 1) (if c == 'a' then a * 3 else a, if c == 'b' then b * 3 else b)
            INC c -> executeInstructionAtIndex instructions (index + 1) (if c == 'a' then a + 1 else a, if c == 'b' then b + 1 else b)
            JMP n -> executeInstructionAtIndex instructions (index + n) (a, b)
            JIE c n -> executeInstructionAtIndex instructions (if even (if c == 'a' then a else b) then index + n else index + 1) (a, b)
            JIO c n -> executeInstructionAtIndex instructions (if (if c == 'a' then a else b) == 1 then index + n else index + 1) (a, b)
