module ValidBraces where

validBraces :: String -> Bool
validBraces xs = null $ calcStack [] xs where
    calcStack stack  []     = stack
    calcStack []     (x:xs) = calcStack [x] xs
    calcStack (s:ss) (x:xs) = if isPair s x then calcStack ss xs else calcStack (x:s:ss) xs
    isPair '(' ')' = True
    isPair '[' ']' = True
    isPair '{' '}' = True
    isPair  _   _  = False




tst = [
      validBraces "[(])"           == False,
      validBraces "()"             == True,
      validBraces "[([)"           == False,
      validBraces "())({}}{()][][" == False,
      validBraces "({})[({})]"     == True
    ]
