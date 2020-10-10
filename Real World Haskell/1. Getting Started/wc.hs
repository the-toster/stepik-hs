main = interact (\input -> show (lineCount input) ++ "\n")

lineCount =  length . lines
inp = "Teignmouth, England\nParis, France\nUlm, Germany\nAuxerre, France\nBrunswick, Germany\nBeaumont-en-Auge, France\nRyazan, Russia";
wordCount input = sum $ map (length . words) (lines input)
tst = wordCount inp
