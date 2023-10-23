type Year = Int
type Month = Int
type Day = Int
type MonthCodes = [Int]

data Date = Date Year Month Day deriving (Show, Eq, Ord)

data Weekday = Sun | Mon | Tue | Wed | Thu | Fri | Sat 
    deriving (Eq, Enum)

instance Show Weekday where
    show Sun = "Sunday"
    show Mon = "Monday"
    show Tue = "Tuesday"
    show Wed = "Wednesday"
    show Thu = "Thursday"
    show Fri = "Friday"
    show Sat = "Saturday"

monthCodesBasic :: MonthCodes
monthCodesBasic = [6, 2, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]

isLeapYear :: Integral a => a -> Bool
isLeapYear y | y `mod` 400 /= 0 && y `mod` 100 == 0 = False
             | y `mod` 4 == 0 = True
             | otherwise = False

monthCodesForYear :: Integral a => a -> MonthCodes
monthCodesForYear y = if isLeapYear y
                      then monthCodesModified
                      else monthCodesBasic
    where 
        monthCodesModified = firstTwoMod ++ afterTwo
        firstTwo = (take 2 monthCodesBasic)
        firstTwoMod = map (flip (-) 1) firstTwo
        afterTwo = (drop 2 monthCodesBasic)

normDay :: Int -> Int
normDay = flip (mod) 7

-- what about 20th century and others. 
-- I suppose there're 4 patterns in centuries
codeYear y = normDay (yNorm `div` 4 + yNorm `mod` 7) 
    where yNorm = y - 2000

weekDayFromDate :: Date -> Weekday
weekDayFromDate (Date y m day) = toEnum dayNum :: Weekday
    where
        year = codeYear y
        monthCode = monthCodesForYear y
        codeMonth codes m = codes !! (m - 1)
        month = codeMonth monthCode m
        dayNum = normDay $ year + month + day

testDate = weekDayFromDate (Date 2023 11 27)

main = do
    putStrLn "Enter Year"
    year <- getLine
    let yearClean = read year :: Int
    putStrLn "Enter numeric month (1-12)"
    month <- getLine
    let monthClean = read month :: Int
    putStrLn "Enter day"
    day <- getLine
    let dayClean = read day :: Int
    let date = Date yearClean monthClean dayClean
    putStr "The day of the week is "
    print $ weekDayFromDate date