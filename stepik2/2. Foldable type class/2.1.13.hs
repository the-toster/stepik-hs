import Data.Monoid

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldr (\x b -> (Endo x) <> b) mempty
