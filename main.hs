module Main where 

data Operation = Suma | Resta | Mult | Div 
    deriving (Eq, Show) 

data Expr = Val Double | Op Operation Expr Expr 
    deriving (Eq, Show) 

main :: IO ()
main = do
    let prueba = Op Mult (Op Suma (Val 8) (Val 2)) (Val 4)
    putStrLn "Expresión:"
    print prueba
    putStrLn "Resultado:"
    print (eval prueba)

aplicarOp :: Operation -> Double -> Double -> Maybe Double 
aplicarOp Suma n1 n2 = Just (n1 + n2) 
aplicarOp Resta n1 n2 = Just (n1 - n2) 
aplicarOp Mult n1 n2 = Just (n1 * n2) 
aplicarOp Div n1 n2 = if n2 == 0 then Nothing else Just (n1 / n2) 

eval :: Expr -> Maybe Double 
eval (Val n) = Just n
eval (Op op izq dir) = 
    case (eval izq, eval dir) of 
        (Just n1, Just n2) -> aplicarOp op n1 n2 
        _ -> Nothing 

sacarUna :: [a] -> [(a, [a])]
sacarUna [] = [] 
sacarUna (x:xs) = (x:xs):[(y, x:resto) | (y, resto) <- sacarUna xs]

sacarDos :: [a] -> [(a, a, [a])]
sacarDos _ = undefined 