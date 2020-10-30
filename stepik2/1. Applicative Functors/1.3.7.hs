import Text.Parsec
import Control.Applicative

ignoreBraces :: Parsec [Char] u a -> Parsec [Char] u b -> Parsec [Char] u c -> Parsec [Char] u c
ignoreBraces openBraces closeBraces contents = openBraces *> contents <* closeBraces
