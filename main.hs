module Main where 

data Operation = Suma | Resta | Mult | Div 
    deriving (Eq) 

data Expr = Val Double | Op Operation Expr Expr 
    deriving (Eq)

instance Show Operation where 
    show Suma = "+"
    show Mult = "*"
    show Div = "/"
    show Resta = "-"

instance Show Expr where
    show (Val n) = 
        let entero = round n
        in if n == fromIntegral entero then show entero else show n 
    show (Op op izq der) = "(" ++ show izq ++ " " ++ show op ++ " " ++ show der ++ ")"  

main :: IO ()
main = do
    putStrLn "BIENVENIDO a 4=10"
    jugar
   

jugar :: IO () 
jugar = do 
    putStrLn "\n----------------"
    putStrLn "Introduce 4 números separados por espacios"
    input <- getLine
    let numeros = leerNumeros input 
    putStrLn ("Buscando soluciones para:" ++ show numeros) 
    let resultados = soluciones numeros 
    if null resultados 
        then putStrLn "No se encontró ninguna solución" 
        else do 
            putStrLn "¡Éxito! Aquí tienes una forma de resolverlo:"
            print (head resultados) 
    putStrLn "------------------------------"
    putStrLn "Quiere jugar de nuevo? (s/n)" 
    respuesta <- getLine 
    if respuesta == "s" || respuesta == "S"
        then jugar 
        else putStrLn "Gracias por jugar!" 



leerNumeros :: String -> [Expr]
-- "8 4 2 6" = lista de expr 
leerNumeros texto = map Val (map read (words texto))

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

--Función combinatoria 
sacarUna :: [a] -> [(a, [a])]
sacarUna [] = [] 
sacarUna (x:xs) = (x, xs):[(y, x:resto) | (y, resto) <- sacarUna xs]

sacarDos :: [a] -> [(a, a, [a])]
sacarDos lista = [(x, y, restoFinal) | (x, resto1) <- sacarUna lista, (y, restoFinal) <- sacarUna resto1]

soluciones :: [Expr] -> [Expr] 
soluciones [] = []
soluciones [e] = 
    case eval e of 
        Just resultado -> if abs (resultado - 10.0) < 0.001 then [e] else [] 
        Nothing -> []
soluciones lista = [sol | (a, b, resto) <- sacarDos lista, op <- [Suma, Resta, Mult, Div], let nuevaExpr = Op op a b, sol <- soluciones (nuevaExpr : resto)]