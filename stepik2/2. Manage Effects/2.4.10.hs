data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

tst1 = Bi 'a' 'b' (Un 'c')
tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
tst3 = Bi 'i' 'j' (Un 'k')
tst = (concat3OC tst1 tst2 tst3) == (Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k'))))))

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un a) (Un b) c            = Bi a b c
concat3OC (Un a) (Bi b c odd1) odd2  = concat3OC (Un a) (Un b) (concat3OC (Un c) odd1 odd2)
concat3OC (Bi a b odd1) odd2 odd3    = concat3OC (Un a) (Un b) (concat3OC odd1 odd2 odd3)
