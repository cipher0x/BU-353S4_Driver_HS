import qualified Data.ByteString.Char8 as B
import System.IO
import System.Hardware.Serialport
import Control.Monad.Loops
import Control.Monad
import Data.Maybe
import Data.Char
import Data.List
import Data.Bits

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
get_valid_line :: IO String
get_valid_line = do --pass s into this function******************
    let port = "/dev/ttyUSB0"  -- Linux
    s <- hOpenSerial port defaultSerialSettings {commSpeed = CS4800,
                                                timeout = 5000}
    ln <- hGetLine s
    if validate_GPGGA_line ln then return ln else get_valid_line 

main :: IO ()
main = do 
  valid_line <- get_valid_line
  print(valid_line)
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
