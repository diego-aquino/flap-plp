module Utils.Strings where

removeLeadingSpaces :: String -> String
removeLeadingSpaces string
  | null string || head string /= ' ' = string
  | otherwise = removeLeadingSpaces (tail string)
