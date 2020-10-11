main = interact (\input -> show (lineCount input) ++ "\n")

lineCount =  length . lines
wordCount input = sum $ map (length . words) (lines input)
charCount inp = sum $ map length (words inp)

inp = "Teignmouth, England\nParis, France\nUlm, Germany\nAuxerre, France\nBrunswick, Germany\nBeaumont-en-Auge, France\nRyazan, Russia";
tst = charCount inp
