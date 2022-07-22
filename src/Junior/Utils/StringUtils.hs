module Junior.Utils.StringUtils where

padR :: Int -> String -> String
padR n s
    | length s < n  = s ++ replicate (n - length s) ' '
    | otherwise     = s