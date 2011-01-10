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
module Main where
import System (getArgs)
import System.Console.GetOpt
import Rot


data Options = Options {
	optInput 	:: IO String,
	optHelp 	:: Bool,
	optOutput	:: (String -> IO ()),
	optFn 		:: (String -> String)
}


defaultOptions	= Options {
	optHelp		= False,
	optInput	= getContents,
	optOutput	= putStr,
	optFn		= rot13
}


options = [ 
	Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True }))
			"display this help and exit",
	Option [] ["to-13"] (NoArg (\opts -> opts { optFn = rot13 }))
			"encode using ROT13 encoding",
	Option [] ["from-13"] (NoArg (\opts -> opts { optFn = unrot13 }))
			"decode using ROT13 encoding",
	Option [] ["to-47"] (NoArg (\opts -> opts { optFn = rot47 }))
			"encode using ROT47 encoding",
	Option [] ["from-47"] (NoArg (\opts -> opts { optFn = unrot47 }))
			"decode using ROT47 encoding",
	Option ['i'] ["input"] (ReqArg (\f opts -> opts { optInput = readFile f }) "FILE")
			"read input from file",
	Option ['o'] ["output"] (ReqArg (\f opts -> opts { optOutput = writeFile f }) "FILE")
			"write output to file"
	]


usage = usageInfo "Usage: rot [OPTIONS...]\n" options


parseOpts	:: [String] -> IO [Options -> Options]
parseOpts args	=
	case getOpt Permute options args of
	    (flags, [], [])	-> return flags
	    (_, nonOpts, [])	-> error $ "unrecognized arguments: " ++ unwords nonOpts
 	    (_, _, msgs)	-> error $ concat msgs ++ usage


processOpts 		:: [Options -> Options] -> Options
processOpts []		= defaultOptions
processOpts opts	= foldl (flip id) defaultOptions opts


main = do optList <- parseOpts =<< getArgs
	  let opts = processOpts optList
	  if optHelp opts then
	  	error usage
		else do inp <- optInput opts
			optOutput opts $ optFn opts $ inp
