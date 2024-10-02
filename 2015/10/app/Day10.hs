module Main where
    import BasePrelude ( (>=>), group, Category(id) )
    
    encode :: [Char] -> [String]
    encode g = [show (length g), take 1 g]
    
    nth :: Int -> [Char]
    nth    n = iterate (group >=> encode >=> BasePrelude.id) "1113222113" !! n
    
    main :: IO ()
    main     = print (length $ nth 40, length $ nth 50)