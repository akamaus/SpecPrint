module Text.SpecPrint(spec_show) where
import qualified Data.Text.IO as T
import qualified Data.Text as T

newtype Tst = Tst String deriving Show

replace_quotes :: String->String
replace_quotes [] = []
replace_quotes ('\"':xs) = "\\\"" ++ replace_quotes xs
replace_quotes (x:xs) = x : replace_quotes xs

spec_show :: Show a => a -> IO ()
spec_show t = let str = read $ '\"' : replace_quotes (show t) ++ "\""
              in T.putStrLn $ T.pack str
