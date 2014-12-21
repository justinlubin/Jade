type Power = Double
type Base = Double
data Function = Constant Double
              | X
              | Sum Function Function
              | Product Function Function
              | Quotient Function Function
              | Polynomial Power Function
              | Sin Function
              | Cos Function
              | Exponential Base Function
              | Log Base Function
              | Undefined
              deriving (Eq)

instance Show Function where
    show (Constant a) = intShow a
    show X = "x"
    show (Sum u v) = (show u) ++ " + " ++ (show v)
    show (Product (Constant (-1)) f) = "-" ++ (show f)
    show (Product f (Constant (-1))) = "-" ++ (show f)
    show (Product (Constant a) f) = (intShow a) ++ (show f)
    show (Product u v) = "(" ++ (show u) ++ ") * (" ++ (show v) ++ ")"
    show (Quotient u v) = "(" ++ (show u) ++ ") / (" ++ (show v) ++ ")"
    show (Polynomial p f)
        | f == X    = (show f) ++ "^" ++ (intShow p)
        | otherwise = "(" ++ (show f) ++ ")^" ++ (intShow p)
    show (Sin f) = "sin(" ++ (show f) ++ ")"
    show (Cos f) = "cos(" ++ (show f) ++ ")"
    show (Exponential b f) = (intShow b) ++ "^" ++ "(" ++ (show f) ++ ")"
    show (Log b f) = "log" ++ (intShow b) ++ "(" ++ (show f) ++ ")"
    show Undefined = "Undefined"

derivative :: Function -> Function
derivative (Constant _) = Constant 0
derivative X = Constant 1
derivative (Sum u v) = Sum (derivative u) (derivative v)
derivative (Product u v) = Sum (Product (derivative u) v) (Product u (derivative v)) 
derivative (Quotient u v) = (Quotient (Sum (Product v (derivative u)) (Product (Product (Constant (-1)) u) (derivative v))) (Polynomial 2 v))
derivative (Polynomial p f) = Product (derivative f) (Product (Constant p) (Polynomial (p - 1) f))
derivative (Sin f) = Product (derivative f) (Cos f)
derivative (Cos f) = Product (derivative f) (Product (Constant (-1)) (Sin f))
derivative (Exponential b f) = Product (derivative f) (Product (Exponential b f) (Log 2.71 (Constant b)))
derivative Undefined = Undefined

simplify :: Function -> Function
simplify (Sum f (Constant 0)) = f
simplify (Sum (Constant 0) f) = f
simplify (Sum (Constant a) (Constant b)) = Constant (a + b)
simplify (Sum (Polynomial 2 (Cos u)) (Polynomial 2 (Sin v))) = (Constant 1)
simplify (Sum (Polynomial 2 (Sin u)) (Polynomial 2 (Cos v))) = (Constant 1)
simplify (Sum u v)
    | u == v    = Product (Constant 2) u 
    | otherwise = Sum (simplify u) (simplify v)
simplify (Product f (Constant 0)) = Constant 0
simplify (Product (Constant 0) f) = Constant 0
simplify (Product f (Constant 1)) = f
simplify (Product (Constant 1) f) = f
simplify (Product (Constant a) (Constant b)) = Constant (a * b)
simplify (Product u v)
    | u == v    = Polynomial 2 u
    | otherwise = Product (simplify u) (simplify v)
simplify (Quotient f (Constant 0)) = Undefined
simplify (Quotient (Constant 0) f) = Constant 0
simplify (Quotient f (Constant 1)) = f
simplify (Quotient u v)
    | u == v    = Constant 1
    | otherwise = Quotient (simplify u) (simplify v)
simplify (Polynomial 1 f) = f
simplify (Polynomial 0 (Constant 0)) = Undefined
simplify (Polynomial _ (Constant 0)) = Constant 0
simplify f = f

simplifyIteration :: Function -> [Function]
simplifyIteration = iterate simplify

fullySimplify :: Function -> Function
fullySimplify f = simplifyIteration f !! 100 

d = fullySimplify . derivative
fs = fullySimplify

evaluate :: Function -> Double -> Double
evaluate (Constant a) x = a
evaluate X x = x
evaluate (Sum u v) x = (evaluate u x) + (evaluate v x)
evaluate (Product u v) x = (evaluate u x) * (evaluate v x)
evaluate (Polynomial p f) x = (evaluate f x) ** p
evaluate (Sin f) x = sin (evaluate f x)
evaluate (Cos f) x = cos (evaluate f x)
evaluate (Exponential b f) x = b ** (evaluate f x)
evaluate (Log b f) x = logBase b (evaluate f x)
evaluate Undefined x = undefined

isInt :: Double -> Double -> Bool
isInt precision x = x - fromInteger (round x) < precision

probablyInt :: Double -> Bool
probablyInt = isInt 0.00000000001

intShow :: Double -> String
intShow x
    | probablyInt x = show . round $ x
    | otherwise     = show x
