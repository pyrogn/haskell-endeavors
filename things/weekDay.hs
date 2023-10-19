monthCodesBasic = [6, 2, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
isLeapYear y | y `mod` 400 /= 0 && y `mod` 100 == 0 = False
             | y `mod` 4 == 0 = True
             | otherwise = False

monthCodesForYear y = if isLeapYear y
                      then monthCodesModified
                      else monthCodesBasic
    where 
        monthCodesModified = map (flip (-) 1) firstTwo ++ afterTwo
        firstTwo = (take 2 monthCodesBasic)
        afterTwo = (drop 2 monthCodesBasic)

codeYear y = (mod) ((yNorm `div` 4) + (yNorm `mod` 7)) 7 where yNorm = y - 2000
codeMonth codes m = codes !! (m - 1)

numForDay y m day = (mod) (year + month + day) 7
    where
        year = codeYear y
        monthCode = monthCodesForYear y
        month = codeMonth monthCode m
