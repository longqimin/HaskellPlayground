{-#LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Csv
import Data.Text (Text)
import Data.Vector hiding (empty, (++))
import Prelude hiding (length)

data Person = Person { name :: !Text, age :: !Int } deriving (Show)

instance FromRecord Person where
     parseRecord v
         | length v == 2 = Person <$>
                           v .! 0 <*>
                           v .! 1
         | otherwise     = empty

instance ToRecord Person where
	toRecord (Person name age) = record [toField name, toField age]

main = do
	let a = Person "John" 18
	let encodeString = encode [a]
	let decodeStrng = decode NoHeader encodeString :: Either String (Vector Person)
	putStrLn $ "encode a : " ++ show encodeString
	putStrLn $ "decode \"John, 18\" : " ++ show decodeStrng
