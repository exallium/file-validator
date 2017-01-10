module Main where

import Lib

isCharCountOk :: String -> Bool
isCharCountOk = (<=80) . length

onCharCountError :: String -> String
onCharCountError s = s ++ " is too long"

validateCharCount = Validator isCharCountOk onCharCountError

isPeriodLastChar :: String -> Bool
isPeriodLastChar s = null s || last s == '.'

onPeriodLastCharError :: String -> String
onPeriodLastCharError s = s ++ " does not end with period"

validatePeriod = Validator isPeriodLastChar onPeriodLastCharError

myValidation = validateFile [validateCharCount, validatePeriod]

main :: IO ()
main = myValidation "/Users/ahart/.vimrc"
