import Data.List
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Monoid
import Data.Maybe (mapMaybe)
import Text.Printf (printf)
import Data.Ratio
import Text.Read (readMaybe)


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


-- Helper function to safely read Int values
readInt :: String -> Maybe Int
readInt = readMaybe


-- Function to find the index of a keyword in the header
findHeaderIndex :: String -> [String] -> Maybe Int
findHeaderIndex keyword headers = lookup keyword (zip headers [0..])


-- Parse a single CSV row into HospitalRecord
parseCSVLine :: [String] -> String -> Either String HospitalRecord
parseCSVLine headers line =
    let fields = splitOn "," line
        lookupField :: String -> Either String String
        lookupField fieldName =
            case findHeaderIndex fieldName headers of
                Just idx -> if idx < length fields
                            then Right (fields !! idx)
                            else Left $ "Field " ++ fieldName ++ " not found in row"
                Nothing -> Left $ "Field " ++ fieldName ++ " not found in header"
    in do
        dateVal          <- lookupField "date"
        stateVal         <- lookupField "state"
        totalBedsVal     <- lookupField "beds" >>= readIntEither
        covidBedsVal     <- lookupField "beds_covid" >>= readIntEither
        noncritBedsVal   <- lookupField "beds_noncrit" >>= readIntEither
        admittedPUIVal   <- lookupField "admitted_pui" >>= readIntEither
        admittedCOVIDVal <- lookupField "admitted_covid" >>= readIntEither
        admittedTotalVal <- lookupField "admitted_total" >>= readIntEither
        dischargedPUIVal <- lookupField "discharged_pui" >>= readIntEither
        dischargedCOVIDVal <- lookupField "discharged_covid" >>= readIntEither
        dischargedTotalVal <- lookupField "discharged_total" >>= readIntEither
        hospitalCOVIDVal <- lookupField "hosp_covid" >>= readIntEither
        hospitalPUIVal   <- lookupField "hosp_pui" >>= readIntEither
        hospitalNonCOVIDVal <- lookupField "hosp_noncovid" >>= readIntEither
        Right $ HospitalRecord dateVal stateVal totalBedsVal covidBedsVal noncritBedsVal
                admittedPUIVal admittedCOVIDVal admittedTotalVal dischargedPUIVal dischargedCOVIDVal
                dischargedTotalVal hospitalCOVIDVal hospitalPUIVal hospitalNonCOVIDVal
  where
    readIntEither :: String -> Either String Int
    readIntEither str = maybe (Left $ "Invalid integer: " ++ str) Right (readInt str)


-- Read and parse CSV file into HospitalRecords
readCSV :: FilePath -> IO [HospitalRecord]
readCSV filePath = do
    content <- lines <$> readFile filePath
    let (header:rows) = content  -- First row is the header
        headers = splitOn "," header
    return $ mapMaybe (parseLine headers) rows
  where
    parseLine headers line = case parseCSVLine headers line of
        Left _ -> Nothing
        Right valid -> Just valid


-- Group records by state
groupState :: [HospitalRecord] -> [[HospitalRecord]]
groupState records =
    let sortedRecords = sortBy (compare `on` state) records
    in groupBy ((==) `on` state) sortedRecords


-- Question 1: Find the state with the highest total beds
maxBedsOfState :: [HospitalRecord] -> String
maxBedsOfState records =
    let maxBed = maximumBy (compare `on` totalBeds) records
    in state maxBed


-- Question 2: Calculate the ratio of COVID beds to total beds
covidToTotalRatio :: [HospitalRecord] -> Ratio Int
covidToTotalRatio records =
    let totalCovidBeds = sum (map covidBeds records)
        totalAllBeds = sum (map totalBeds records)
    in fromIntegral totalCovidBeds % fromIntegral totalAllBeds
   

-- Question 3: Calculate averages of admissions by state
averageAdmissions :: [HospitalRecord] -> [(String, (Double, Double))]
averageAdmissions records =
    let groupedRecords = groupState records
    in map calculateAverages groupedRecords
  where
    calculateAverages group =
        let stateName = state (head group)
            totalRecords = length group
            avgPUI = fromIntegral (sum (map admittedPUI group)) / fromIntegral totalRecords
            avgCOVID = fromIntegral (sum (map admittedCOVID group)) / fromIntegral totalRecords
        in (stateName, (avgPUI, avgCOVID))


-- Main function
main :: IO ()
main = do
    -- readCSV "C:/Users/sumho/haskelprojectG1/Assignment2/app/hospital.csv"  >>= \records ->

    --Q1 
    -- putStrLn "\nQuestion 1: which state has the highest total of hospital bed"
    -- (case maxBedsOfState records of
    --     Just state -> putStrLn "\nState with maximum beds: \n" ++ state
    --     Nothing -> putStrLn "Could not find the state")
    let filePath = "C:/Users/sumho/haskelprojectG1/Assignment2/app/hospital.csv"
    records <- readCSV filePath

    -- Question 1
    let maxBeds = maxBedsOfState records
    putStrLn $ "State with the highest total beds: " ++ maxBeds

    -- Question 2
    let ratio = covidToTotalRatio records
    putStrLn $ "Ratio: " ++ show (numerator ratio) ++ ":" ++ show (denominator ratio)
    printf "Ratio in decimal: %.2f\n" (fromRational (toRational ratio) :: Double)

    -- Question 3
    let averages = averageAdmissions records
    putStrLn "Average admissions by state (PUI, COVID, Total):"
    mapM_ (\(stateName, (avgPUI, avgCOVID)) ->
        putStrLn $ stateName ++ ": PUI=" ++ printf "%.2f" avgPUI ++ ", COVID=" ++ printf "%.2f" avgCOVID) averages