import Data.Ratio

data Number = NumberInt Integer
            | NumberMcDouble Double
            | NumberE
            | NumberRatio (Ratio Integer)
            | Ap Fun Number
            deriving (
              Show, Eq
            )

data Fun = Diff Fun
         | Expt Number
         | InvExpt Number
         | Const Number
         | Add Number
         | Integrate Fun
         | Compose Fun Fun
         | Mul Number
         | Div Number -- \m n -> m / n
--         | If (Number -> Bool) Number Number
         | Negate
         | Ceil
         | Floor
         | Sin
         | Cos
         | Tan
         | Csc
         | Id
         | Log Number
         | FSum Fun Fun -- \g f x -> gx + g x
         | FMul Fun Fun -- \g f x -> g x * f x
         | FExp Fun Fun -- \g f x -> g x ^ f x
         | FDiv Fun Fun -- \g f x -> g x / f x
         deriving (
           Show, Eq
         )

s :: Fun -> Fun
s (Compose (Expt b) (Expt a)) = Expt $ Ap (Mul b) a
s (Compose Id f) = s f
s (Compose f g) = Compose (s f) (s g)
s (Diff (Log NumberE)) = Id
s (Diff (InvExpt NumberE)) = InvExpt NumberE
s (Diff Sin) = Cos
s (Diff (Const _)) = Const $ NumberInt 0
s (Diff Cos) = Compose Negate Sin
s (Diff (Compose f g)) = s $ FMul (s $ Diff g) $ Compose (s $ Diff f) (s g)
s (Diff (Expt n)) = s $ Compose (Mul n) $ Expt $ Ap (Add $ NumberInt $ -1) n
s (Diff (FSum f g)) = s $ FSum (Diff f) $ Diff g
s (Diff (FMul f g)) = s $ FSum (FMul (Diff f) g) $ FMul (Diff g) f
s (Diff (FExp f g)) = s $ Diff $ Compose (InvExpt NumberE) $ FMul g $ Compose (Log NumberE) f
s (Diff (FDiv f g)) = s $ FDiv (FSum (FMul (Diff f) g) (FMul f $ Diff g)) (Compose (Expt $ NumberInt 2) g)
s (Diff x) = Diff $ s x
s (FMul g f) = FMul (s g) (s f)
s (FSum g f) = FSum (s g) (s f)
s (FExp g f) = FExp (s g) (s f)
s (FDiv g f) = FDiv (s g) (s f)
s x = x

s2 :: Fun -> Fun
s2 f = if f' == f then f else s2 f'
  where
  f' = s f

sExp :: Number -> Number
sExp (NumberInt n) = NumberInt n;
sExp (NumberMcDouble n) = NumberMcDouble n;
sExp (NumberRatio n) = NumberRatio n;
sExp NumberE = NumberE;
sExp (Ap (Add (NumberInt m)) (NumberInt n)) = NumberInt $ n + m
sExp (Ap (Mul (NumberInt m)) (NumberInt n)) = NumberInt $ m * n
sExp (Ap (Expt (NumberInt m)) (NumberInt n)) = NumberInt $ n ^ m
sExp (Ap (Div (NumberInt m)) (NumberInt n)) = NumberRatio $ n % m

toMcDouble :: Number -> Double
toMcDouble (NumberInt n) = fromIntegral n
toMcDouble (NumberMcDouble n) = n
toMcDouble (NumberE) = exp 1
toMcDouble (NumberRatio n) = fromIntegral (numerator n) / fromIntegral (denominator n)
toMcDouble (Ap (Diff _) _) = error "We still can't apply differentials to constants."
toMcDouble (Ap (Integrate _) _) = error "We still can't apply integrals to constants."
toMcDouble (Ap (Const m) n) = toMcDouble m
toMcDouble (Ap (InvExpt m) n) = toMcDouble m ** toMcDouble n
toMcDouble (Ap (Expt m) n) = toMcDouble n ** toMcDouble m
toMcDouble (Ap (Add m) n) = toMcDouble m + toMcDouble n
toMcDouble (Ap (Mul m) n) = toMcDouble m * toMcDouble n
toMcDouble (Ap (Div m) n) = toMcDouble m / toMcDouble n
toMcDouble (Ap Negate n) = - (toMcDouble n)
toMcDouble (Ap Ceil n) = fromIntegral $ ceiling $ toMcDouble n
toMcDouble (Ap Floor n) = fromIntegral $ floor $ toMcDouble n
toMcDouble (Ap Sin n) = sin $ toMcDouble n
toMcDouble (Ap Cos n) = cos $ toMcDouble n
toMcDouble (Ap Tan n) = tan $ toMcDouble n
-- toMcDouble (Ap Csc n) = csc $ toMcDouble n
toMcDouble (Ap Id n) = toMcDouble n
toMcDouble (Ap (Log m) n) = log (toMcDouble n) / log (toMcDouble m)
toMcDouble (Ap (FSum g f) n) = toMcDouble (Ap g n) + toMcDouble (Ap f n)
toMcDouble (Ap (FMul g f) n) = toMcDouble (Ap g n) * toMcDouble (Ap f n)
toMcDouble (Ap (FExp g f) n) = toMcDouble (Ap g n) ** toMcDouble (Ap f n)
toMcDouble (Ap (FDiv g f) n) = toMcDouble (Ap g n) / toMcDouble (Ap f n)
--toMcDouble (Ap (FDiv g f) n) = toMcDouble (Ap g n) / toMcDouble (Ap f n)
toMcDouble (Ap (Compose g f) n) = toMcDouble $ Ap g $ NumberMcDouble $ toMcDouble $ Ap f $ NumberMcDouble $ toMcDouble n
-- Ap (Compose (Mul (NumberInt 2)) (Expt (Ap (Add (NumberInt (-1))) (NumberInt 2)))) (NumberInt 5)
