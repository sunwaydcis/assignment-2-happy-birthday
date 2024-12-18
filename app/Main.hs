import Data.List
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Monoid
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Data.Ratio
import Text.Read (reads)


-- Define HospitalRecord data type
data HospitalRecord = HospitalRecord
    { date :: String
    , state :: String
    , totalBeds :: Int
    , covidBeds :: Int
    , noncritBeds :: Int
    , admittedPUI :: Int
    , admittedCOVID :: Int
    , admittedTotal :: Int
    , dischargedPUI :: Int
    , dischargedCOVID :: Int
    , dischargedTotal :: Int
    , hospitalCOVID :: Int
    , hospitalPUI :: Int
    , hospitalNonCOVID :: Int
    } deriving Show


-- Function to find the index of a keyword in the header by using zip to assign field name with idx and use lookup function
{-
REFERENCES:
1. (lookup): https://zvon.org/other/haskell/Outputprelude/lookup_f.html
2. (zip): http://www.zvon.org/other/haskell/Outputprelude/zip_f.html
-}
findHeader :: String -> [String] -> Maybe Int
findHeader keyword headers = lookup keyword (zip headers [0..])


-- Parse a single data row into HospitalRecord
-- Extracts values based on the headers and convert string into numbers
{- 
REFERENCES:
1. (Reads): https://downloads.haskell.org/~ghc/6.6.1/docs/html/libraries/base/Text-Read.html
2. (Reads): http://www.zvon.org/other/haskell/Outputprelude/ReadS_d.html
-}
parseCSVLine :: [String] -> String -> Either String HospitalRecord
parseCSVLine headers line =
    let fields = splitOn "," line
        lookupField :: String -> Either String String
        lookupField fieldName =
            case findHeader fieldName headers of
                Just idx
                        | idx < length fields -> Right (fields !! idx) --checks to ensure that the index value is not out of bounds 
                        | otherwise -> Left $ "Field " ++ fieldName ++ " not found in row" --returns error message that field is not found in row
                Nothing -> Left $ "Field " ++ fieldName ++ " not found in header" --returns error message that field is not found in header
  
        --To check whether string is able to convert into integer by using reads as return a list of possible values in tuple
        readInt str = 
            case reads str :: [(Int, String)] of
                [(n, "")] -> Right n 
                _         -> Left $ "Fail to convert into integer: " ++ str  --returns error message if the string cannot be coverted into integer

        --Function to match the field and parse into hospital record
        lookupAndParse fieldName = lookupField fieldName >>= readInt

        --Combines all fields to construct a HospitalRecord records
        parseRecord = HospitalRecord <$> lookupField "date"
            <*> lookupField "state"
            <*> lookupAndParse "beds"
            <*> lookupAndParse "beds_covid"
            <*> lookupAndParse "beds_noncrit"
            <*> lookupAndParse "admitted_pui"
            <*> lookupAndParse "admitted_covid"
            <*> lookupAndParse "admitted_total"
            <*> lookupAndParse "discharged_pui"
            <*> lookupAndParse "discharged_covid"
            <*> lookupAndParse "discharged_total"
            <*> lookupAndParse "hosp_covid"
            <*> lookupAndParse "hosp_pui"
            <*> lookupAndParse "hosp_noncovid"
    in parseRecord

{-
REFERENCES:
1. (mapMaybe): http://www.zvon.org/other/haskell/Outputmaybe/mapMaybe_f.html
2. (splitOn): https://hackage.haskell.org/package/text-2.1.2/docs/Data-Text.html#v:splitOn
-}
-- Read and parse dataset into HospitalRecords
readCSV :: FilePath -> IO [HospitalRecord]
readCSV filePath = do
    content <- lines <$> readFile filePath
    let (header:rows) = content         -- sperate header from data rows
        headers = splitOn "," header    --split header into field names
    return $ mapMaybe (parseLine headers) rows
  where
    parseLine headers line = case parseCSVLine headers line of
        Left _ -> Nothing
        Right valid -> Just valid


-- Group records by state
{-
REFERENCES:
1. (GroupBy): https://hackage.haskell.org/package/groupBy-0.1.0.0/docs/Data-List-GroupBy.html
2. (GroupBy): http://www.zvon.org/other/haskell/Outputlist/groupBy_f.html
3. (Ratio): https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Ratio.html
4. (MaximumBy): https://stackoverflow.com/questions/44832978/haskell-maximumby-for-ord-instances
5. (`on`): https://stackoverflow.com/questions/45261292/understanding-data-function-on-type-signature 
-}
groupState :: [HospitalRecord] -> [[HospitalRecord]]
groupState records =
    let sortedRecords = sortBy (compare `on` state) records
    in groupBy ((==) `on` state) sortedRecords


-- Question 1: Find the state with the highest total beds
--
maxBedsOfState :: [HospitalRecord] -> String
maxBedsOfState records =
    let maxBed = maximumBy (compare `on` totalBeds) records --compare the totalBeds from all records and find the record with the highest value
    in state maxBed --takes the state value from the record with highest totalBeds


-- Question 2: Calculate the ratio of COVID beds to total beds
--
covidToTotalRatio :: [HospitalRecord] -> Ratio Int
covidToTotalRatio records =
    let totalCovidBeds = sum (map covidBeds records) --sums up all the beds_covid values 
        totalAllBeds = sum (map totalBeds records) --sums up all the beds values
    in fromIntegral totalCovidBeds % fromIntegral totalAllBeds --converted into a rational number to show ratio 
   

-- Question 3: Calculate averages of admissions by state
-- group the records by state and calculate average for each state group
averageAdmissions :: [HospitalRecord] -> [(String, (Double, Double))]
averageAdmissions records =
    let groupedRecords = groupState records
    in map calculateAverages groupedRecords
  where
    calculateAverages group =
        let stateName = state (head group)
            totalRecords = length group
            avgPUI = fromIntegral (sum (map admittedPUI group)) / fromIntegral totalRecords --calculate the average of PUI admissions
            avgCOVID = fromIntegral (sum (map admittedCOVID group)) / fromIntegral totalRecords --calculate the average of COVID admissions 
        in (stateName, (avgPUI, avgCOVID))

{-
REFERENCES:
1.(printf): https://stackoverflow.com/questions/7828072/how-does-haskell-printf-work 
-}
-- Main function
main :: IO ()
main =
     readCSV "Assignment2/hospital.csv" >>= \records ->

        --Question 1
        putStrLn ("Question 1:\n" <> "State with the highest total of beds: " <> maxBedsOfState records) >>

        --Question 2
        putStrLn ("\nQuestion 2:\n" <> "Ratio (COVID beds : Total beds): " <>
            show (numerator (covidToTotalRatio records)) <> ":" <>
            show (denominator (covidToTotalRatio records))) >>
        printf "Ratio in decimal: %.2f\n"
            (fromRational (toRational (covidToTotalRatio records)) :: Double) >> --converts into a Double to show the ratio in decimal form

        -- Question 3 
        {-
        REFERENCES:
        1. (Tail Recursion): https://www.programmerinterview.com/recursion/tail-recursion/
        -}
        putStrLn "\nQuestion 3: Average admissions by state (PUI, COVID):" >>
        printAverageAdmissions (averageAdmissions records)
        where
            -- Recursive function to print each state's average admissions
            printAverageAdmissions :: [(String, (Double, Double))] -> IO ()
            printAverageAdmissions [] = return ()  -- Base case: no more records to print
            printAverageAdmissions ((stateName, (avgPUI, avgCOVID)):xs) =
                putStr stateName >>
                putStr ": PUI = " >> printf "%.2f" avgPUI >>
                putStr ", COVID = " >> printf "%.2f\n" avgCOVID >>
                printAverageAdmissions xs
