module Lib
    ( validateLine, validateFile, ValidationResult(Success,Failure), Validator(Validator)
    ) where

data Validator = Validator (String -> Bool) (String -> String)
data ValidationResult = Success | Failure String deriving Show

appendValidations :: ValidationResult -> ValidationResult -> ValidationResult
appendValidations Success Success = Success
appendValidations (Failure x) Success = Failure x
appendValidations Success (Failure x) = Failure x
appendValidations (Failure x) (Failure y) = Failure (x ++ "\n" ++ y)

instance Monoid ValidationResult where
  mappend = appendValidations
  mempty = Success

runValidator :: Validator -> String -> ValidationResult
runValidator (Validator b e) s = if b s then Success else Failure $ e s

validateForInput :: String -> [Validator] -> [ValidationResult]
validateForInput s = map (`runValidator` s)

-- String -> ValidationResult
validateLine :: [Validator] -> String -> ValidationResult
validateLine fs s = foldl mappend mempty $ validateForInput s fs

validateAll :: [Validator] -> String -> [ValidationResult]
validateAll fs fin = map (validateLine fs) (lines fin)

validateFile :: [Validator] -> FilePath -> IO ()
validateFile fs fp = do
  input <- readFile fp
  print (foldl mappend mempty $ validateAll fs input)
