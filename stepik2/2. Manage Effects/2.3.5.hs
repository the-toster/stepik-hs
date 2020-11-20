import Data.Functor.Compose

-- sequenceA . fmap Compose == Compose . fmap sequenceA . sequenceA
-- Compose . fmap sequenceA . sequenceA ::  t (f (g a)) -> Compose f g (t a)
-- sequenceA :: t (f a) -> f (t a)
-- Compose :: f (g a) -> Compose f g a

\x -> (Compose . (fmap sequenceA) . sequenceA) x




\x -> Compose ((fmap sequenceA) (sequenceA x))


 f (t (g a)) -> f (g (t a))
