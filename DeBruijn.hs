module Main where

import System.IO.Unsafe (unsafePerformIO)

data LC = V Int | L LC | App LC LC deriving (Eq, Show)

lId = L (V 0)
lTrue = L (L (V 1))
lFalse = L (L (V 0))

lNot = L (L (L (App (App (V 2) (V 0)) (V 1))))
lAnd = L (L (L (L (App (App (V 3) (App (App (V 2) (V 1)) (V 0))) (V 0)))))
lOr = L (L (L (L (App (App (V 3) (V 1)) (App (App (V 2) (V 1)) (V 0))))))

{-

true  = \x.\y.x
false = \x.\y.y
not   = \b.\x.\y.b y x
and   = \b1.\b2.\x.\y.b1 (b2 x y) y
or    = \b1.\b2.\x.\y.b1 x (b2 x y)

--

true  = LL.1
false = LL.0
not   = LLL.2 0 1
and   = LLLL.3 (2 1 0) 0
or    = LLLL.3 1 (2 1 0)

(LLL.2 0 1) (LL.0)

-}

shift d c (V k) = if k < c then V k else V (k + d)
shift d c (L t1) = L (shift d (c + 1) t1)
shift d c (App t1 t2) = App (shift d c t1) (shift d c t2)

sub j s (V k) = if k == j then s else V k
sub j s (L t1) = L (sub (j + 1) (shift 1 0 s) t1)
sub j s (App t1 t2) = App (sub j s t1) (sub j s t2)

normal (V _) = True
normal (L t) = normal t
normal (App (L _) _) = False
normal (App t1 t2) = normal t1 && normal t2

beta (App (L t12) v2) = Just $ shift (-1) 0 (sub 0 (shift 1 0 v2) t12)
beta (App t1 t2) | normal t1 = App t1 <$> beta t2
beta (App t1 t2) = App <$> beta t1 <*> pure t2
beta (L t1) = L <$> beta t1
beta _ = Nothing

display (Just t) = do
  putStr . show $ t
  getLine
  display (beta t)
display _ = pure ()

main = display . Just $ App lNot lTrue
