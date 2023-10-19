
corpus = [
    "Crock Pot Pasta Never boil pasta again", 
    "Pasta Pomodoro Fresh ingredients Parmesan to taste"
    ]
s1 = corpus !! 1

-- splitBy delimiter = foldr f [[]] 
--             where f c l@(x:xs) | c == delimiter = []:l
--                              | otherwise = (c:x):xs 
-- https://stackoverflow.com/questions/4503958/what-is-the-best-way-to-split-a-string-by-a-delimiter-functionally

splitBy :: Char -> String -> [String]
splitBy _ "" = [];
splitBy delimiterChar inputString = foldr f [""] inputString
  where f :: Char -> [String] -> [String]
        f currentChar allStrings@(partialString:handledStrings)
          | currentChar == delimiterChar = "":allStrings -- start a new partial string at the head of the list of all strings
          | otherwise = (currentChar:partialString):handledStrings -- add the current char to the partial string

splitted = splitBy ' ' s1 

