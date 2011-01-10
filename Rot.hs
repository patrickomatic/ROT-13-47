{-
    ROT 13/47 Encoder
    Copyright (C) 2008 Patrick Carroll <patrick@patrickomatic.com>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Rot where

import Data.Map
import qualified Data.Map as Map
import Char


asciiAlphabet	= "!\"#$%&'()*+,-./" ++ ['0'..'9'] ++ ":;<=>?@" ++ ['A'..'Z'] ++ "[\\]^_`" ++ ['a'..'z'] ++ "{|}~"

rot13Alphabet	= ['a' .. 'z']


shiftStr :: Int -> String -> String
shiftStr n str	= let (a, b) = splitAt n str
		  in b ++ a


rot47 :: String -> String
rot47 str	= let a = asciiAlphabet
		  in rot str a (shiftStr 47 a)

unrot47	:: String -> String
unrot47 str	= let a = asciiAlphabet
		  in rot str (shiftStr 47 a) a


rot13 :: String -> String
rot13 str	= rot str (appendMap toUpper rot13Alphabet) 
		          (appendMap toUpper (shiftStr 13 rot13Alphabet))

unrot13	:: String -> String
unrot13 str	= rot str (appendMap toUpper (shiftStr 13 rot13Alphabet))
			  (appendMap toUpper rot13Alphabet) 


appendMap :: (a -> a) -> [a] -> [a]
appendMap fn s 	= s ++ (fmap fn s)


rot	:: String -> String -> String -> String
rot str a b	= [ case (Map.lookup c lookupTable) of 
					Nothing 	-> c 
					Just x 	-> x
		    | c <- str ]
		where lookupTable = Map.fromList $ zip a b
