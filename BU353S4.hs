import qualified Data.ByteString.Char8 as B
import System.IO
import System.Hardware.Serialport
import Control.Monad.Loops
import Control.Monad
import Data.Maybe
import Data.Char
import Data.List
import Data.Bits
import Data.List.Split

-- TO DO, make sure string contains '*' and two hex digits afterwards before passing to parseHex
--  create data type / type class and return it or maby use tuples?

stringToIntList :: String -> [Int]
stringToIntList line
  | ((>2) . length) line = map ord line
  | otherwise = []

--convert ascii char to hex value
hexChar :: Char -> Int
hexChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $ elemIndex ch "0123456789ABCDEF"

--convert hex string to int
parseHex :: String -> Int
parseHex hex = foldl' f 0 hex where
    f n c = 16*n + hexChar c

--get index of  asterisk char *, ascii 42
indexOfAsterisk :: [Int]->Int
indexOfAsterisk xs = fromMaybe (-1) $ elemIndex 42 xs

--cut list at *
cutListAsterisk :: [Int]->[Int]
cutListAsterisk xs = take (indexOfAsterisk xs) xs

--calculate  line checksum
calculate_GPGGA_checksum :: [Int]->Int
calculate_GPGGA_checksum [] = 0        
calculate_GPGGA_checksum (x:xs) | x == 41 = 0
                                | x /= 36 = x `xor` (calculate_GPGGA_checksum xs)
                                | otherwise = calculate_GPGGA_checksum xs

--calculate checksum from line string
str_GPGGA_checksum :: [Char]->Int
str_GPGGA_checksum xs = calculate_GPGGA_checksum $ cutListAsterisk $ stringToIntList xs

--get sublist containg reported checksum
get_reported_checksum_subList :: [Char]->[Char]
get_reported_checksum_subList xs = [xs!!((indexOfAsterisk $ stringToIntList xs)+1), xs!!((indexOfAsterisk $ stringToIntList xs)+2)] 

--get reported checksum value from GPGGA line
get_reported_checksum :: [Char]->Int
get_reported_checksum xs = parseHex  $ get_reported_checksum_subList xs

--find sub string in string
find_string :: (Eq a) => [a] -> [a] -> Int
find_string search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

--is GPGGA line
is_GPGGA_line :: [Char]->Bool
is_GPGGA_line xs = if (find_string "GPGGA" xs) == (-1) then False else True 

--validate GPGGA line
validate_GPGGA_line :: [Char]->Bool
validate_GPGGA_line xs = if contain_asterisk xs && is_GPGGA_line xs then if get_reported_checksum xs == str_GPGGA_checksum xs then True else False else False
--if get_reported_checksum xs == str_GPGGA_checksum xs then True else False

--conatins aserisk
contain_asterisk :: [Char]->Bool
contain_asterisk xs = '*' `elem` xs

--if x == 42 
--if x /= 36 then x `xor` (calculate_GPGGA_checksum xs) else calculate_GPGGA_checksum xs

get_GPGGA_line :: String -> String
get_GPGGA_line line | "$GPGGA" `isPrefixOf` line = line
                    | otherwise = []

clean_print :: String -> IO ()
clean_print str | str == ""  = putChar '\r'
                | otherwise = print str

--get a valid GPGGA line
--get_valid_line :: IO String
get_valid_line :: Handle -> IO String
get_valid_line s = do --pass s into this function******************
    --let port = "/dev/ttyUSB0"  -- Linux
    --s <- hOpenSerial port defaultSerialSettings {commSpeed = CS4800,
    --                                            timeout = 5000}
    ln <- hGetLine s
    if validate_GPGGA_line ln then return ln else get_valid_line s 




-------------------------------------------------------------------------------
----------------Functions to parse valid GPGGA line----------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- @brief returns GGA - Global Positioning System fix data record type
-- @param string of a GPGGA valid line
-- @return string containing GGA
get_system_fix_data :: [Char] -> [Char]
get_system_fix_data xs = s!!0
                    where s = splitOn "," xs

-- @breif return utc time
-- @param string of a GPGGA valid line
-- @return string utc [hhmmss.ss]
get_utc_time :: [Char] -> [Char]
get_utc_time xs = s!!1
            where s = splitOn "," xs

-- @brief return Latitude
-- @param string of a GPGGA valid line
-- @return string latitude in the form [DDMM.MMMMMM]
get_latitude :: [Char] -> [Char]
get_latitude xs = s!!2
            where s = splitOn "," xs

-- @breif return  latitude direction
-- @param string of a GPGGA valid line
-- @return string latitude direction E: for East, W: for West
get_latitude_direction :: [Char] -> [Char]
get_latitude_direction xs = s!!3 
                    where s = splitOn "," xs

-- @breif return Longitude
-- @param string of a GPGGA valid line
-- @return string longitude in [DDDMM.MMMMMM]
get_longitude :: [Char] -> [Char]
get_longitude xs = s!!4
                where s = splitOn "," xs

-- @breif return Longitude direction
-- @param string of a GPGGA valid line
-- @return string longitutde direction, E: for East, W: for West
get_longitude_direction :: [Char] -> [Char]
get_longitude_direction xs = s!!5
                        where s = splitOn "," xs

-- @brief returns fix quaility
-- @param string of a GPGGA valid line
-- @return string of fix quiality, values below
-- GPS Quality Indicator
--    0: fix not available
--    1: GPS fix
--    2: Differential GPS fix
--    4: Real-Time Kinematic, fixed integers
--    5: Real-Time Kinematic, float integers
get_fix_quaility :: [Char] -> [Char]
get_fix_quaility xs = s!!6
                where s = splitOn "," xs

-- @brief returns Number of GPS satellites being used [0 - 12]
-- @param string of a GPGGA valid line
-- @return string of used gpg satellites values: [0 - 12] 
get_tracked_satellites :: [Char] -> [Char]
get_tracked_satellites xs = s!!7
                        where s = splitOn "," xs

-- @brief returns horizontal dilution of precision of fix
-- @param string of a GPGGA valid line
-- @return string dilution
get_horizontal_dilution :: [Char] -> [Char]
get_horizontal_dilution xs = s!!8
                        where s = splitOn "," xs

-- @brief returns altitude Above Mean Sea
-- @param string of a GPGGA valid line
-- @return string altitude above mean sea
get_altitude_above_mean_sea :: [Char] -> [Char]
get_altitude_above_mean_sea xs = s!!9
                            where s = splitOn "," xs

-- @breif returns units of altitude above mean sea
-- @param string of a GPGGA valid line
-- @return string of Units ex, M for meters
get_altitude_above_mean_sea_units :: [Char] -> [Char]
get_altitude_above_mean_sea_units xs = s!!10
                                where s = splitOn "," xs

-- @brief returns Geoidal separation , the diff between WGS-84 earth ellipsoid and mean sea geoid
-- @param string of a GPGGA valid line
-- @return string diff btw wgs-84 geoid and abms sea geoid
get_height_of_geoid :: [Char] -> [Char]
get_height_of_geoid xs = s!!11
                    where s = splitOn "," xs

-- @breif get height of geoid units
-- @param string of a GPGGA valid line
-- @return string of units ex, M for meters
get_height_of_geoid_units :: [Char] -> [Char]
get_height_of_geoid_units xs = s!!12
                        where s = splitOn "," xs


--function to test get functions
test_get :: [Char] -> [Char]
test_get xs = "Valid Line: " ++ xs ++ "\n" ++ "System Fix: " ++ get_system_fix_data xs ++ "\n" ++ "Time: " ++ get_utc_time xs ++ "\n" ++
    "Latitude: " ++ get_latitude xs ++ " " ++ get_latitude_direction xs ++ "\n" ++ "Longitude: " ++ get_longitude xs ++ " " ++ 
    get_longitude_direction xs ++ "\n" ++ "Fix Quality: " ++ get_fix_quaility xs ++ "\n" ++ "Tracked Satelites: " ++ get_tracked_satellites xs ++
    "\n" ++ "Horizontal Dilution: " ++ get_horizontal_dilution xs ++ "\n" ++ "Altitude AMS: " ++ get_altitude_above_mean_sea xs ++ 
    " " ++ get_altitude_above_mean_sea_units xs ++ "\n" ++ "Height of Geoid: " ++ get_height_of_geoid xs ++ " " ++ get_height_of_geoid_units xs ++ "\n"

main :: IO ()
main = do
  let port = "/dev/ttyUSB0"  -- Linux
  s <- hOpenSerial port defaultSerialSettings {commSpeed = CS4800, timeout = 5000}
--  valid_line <- get_valid_line s
  replicateM_ 100 (get_valid_line s >>= putStrLn . test_get)
  hClose s
--  print(valid_line)
  --let port = "/dev/ttyUSB0"  -- Linux
  --s <- hOpenSerial port defaultSerialSettings {commSpeed = CS4800,
  --                                            timeout = 5000}
                                              --- The Timeout is given in tenth of seconds. 
                                              --- This means, that the program waits 100 ms for a
                                              --- character, until it throws an "End of File reached" error
  --replicateM_ 100 (hGetLine s >>= (clean_print . get_GPGGA_line))
  --ln <- hGetLine s
  --unless (validate_GPGGA_line ln) $ do
  --when (not $ validate_GPGGA_line ln) $ do
  --print(ln)
  --main
  --ln <- hGetLine s
  --print("OutSide")
  --print(ln)
  --if validate_GPGGA_line ln then print(ln) else print("Invalid Line")
  --hClose s
