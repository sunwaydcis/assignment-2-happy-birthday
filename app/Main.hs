import Data.List
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.Monoid
import Data.Maybe (mapMaybe)
import Control.Applicative (liftA2)


-- Define HospitalRecord for aggregation
data HospitalRecord = HospitalRecord
    { state :: String
    , totalBeds :: Sum Int
    , covidBeds :: Sum Int
    , admittedPUI :: Sum Int
    , admittedCOVID :: Sum Int
    , admittedTotal :: Sum Int
    } deriving Show

-- Semigroup and Monoid instances for aggregation
instance Semigroup HospitalRecord where
    r1 <> r2 = HospitalRecord
        { state = state r1
        , totalBeds = totalBeds r1 <> totalBeds r2
        , covidBeds = covidBeds r1 <> covidBeds r2
        , admittedPUI = admittedPUI r1 <> admittedPUI r2
        , admittedCOVID = admittedCOVID r1 <> admittedCOVID r2
        , admittedTotal = admittedTotal r1 <> admittedTotal r2
        }

instance Monoid HospitalRecord where
    mempty = HospitalRecord "" mempty mempty mempty mempty mempty

-- Parse a single CSV row into HospitalRecord
parseCSVLine :: String -> Either String HospitalRecord
parseCSVLine line =
    let fields = splitOn "," line
    in if length fields < 8
        then Left $ "Invalid row: " ++ line
        else Right $ HospitalRecord
            (fields !! 1)  -- State
            (Sum $ read (fields !! 2) :: Sum Int)  -- Total Bed
            (Sum $ read (fields !! 3) :: Sum Int)  -- Total Bed Covid
            (Sum $ read (fields !! 5) :: Sum Int)  -- Admitted PUI
            (Sum $ read (fields !! 6) :: Sum Int)  -- Admitted COVID
            (Sum $ read (fields !! 7) :: Sum Int)  -- Admitted Total

-- Read and parse CSV file into HospitalRecords
readCSV :: FilePath -> IO [HospitalRecord]
readCSV filePath = do
    content <- lines <$> readFile filePath
    let rows = tail content  -- Skip header
    return $ mapMaybe (either (const Nothing) Just . parseCSVLine) rows

-- Group HospitalRecords by state and calculate averages;
groupAndCalculateAverages :: [HospitalRecord] -> [(String, (Double, Double, Double))]
groupAndCalculateAverages records =
    let grouped = groupBy ((==) `on` state) $ sortBy (compare `on` state) records
    in map calculateAverages grouped
  where
    calculateAverages :: [HospitalRecord] -> (String, (Double, Double, Double))
    calculateAverages recordsForState =
        let aggregated = mconcat recordsForState
            totalRecords = fromIntegral $ length recordsForState  -- Convert to Double for division
            avgPUI = fromIntegral (getSum (admittedPUI aggregated)) / totalRecords
            avgCOVID = fromIntegral (getSum (admittedCOVID aggregated)) / totalRecords
            avgTotal = fromIntegral (getSum (admittedTotal aggregated)) / totalRecords
        in (state (head recordsForState), (avgPUI, avgCOVID, avgTotal))

-- Main function
main :: IO ()
main = do
    let filePath = "C:/Users/sumho/haskelprojectG1/Assignment2/app/hospital.csv"  -- Replace with your actual path
    hospitalData <- readCSV filePath

    --Question 2
    -- Sum up all beds and beds_covid from each row
    let totalCovidBeds = getSum (mconcat(map covidBeds hospitalData))
        totalBeds' = getSum (mconcat(map totalBeds hospitalData))

    -- Display total amount of beds for beds_covid and beds
    putStrLn $ "Total Covid Beds: " ++ show totalCovidBeds
    putStrLn $ "Total Beds: " ++ show totalBeds'
    putStrLn $ "Total Covid Beds | Total Beds Ratio: "
    putStrLn $ show totalCovidBeds ++ ":" ++ show totalBeds'


    --Question3
    let averages = groupAndCalculateAverages hospitalData
    
    -- User interaction
    putStrLn "Enter a command (e.g., 'Average of covid', 'Average of pui'):"
    command <- getLine

    case command of
        "Average of covid" -> printAverages averages (\(_, avgCOVID, _) -> avgCOVID)
        "Average of pui"   -> printAverages averages (\(avgPUI, _, _) -> avgPUI)
        "Average of total" -> printAverages averages (\(_, _, avgTotal) -> avgTotal)
        _ -> putStrLn "Unknown command. Please enter 'Average of covid', 'Average of pui', or 'Average of total'."
            

  where
    printAverages :: [(String, (Double, Double, Double))] -> ((Double, Double, Double) -> Double) -> IO ()
    printAverages averages selector =
        mapM_ (\(stateName, avgTuple) -> putStrLn $ stateName ++ ": " ++ show (selector avgTuple)) averages