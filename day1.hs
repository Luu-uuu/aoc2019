import System.IO
import System.Environment
import Numeric

fuel :: Double -> Int
fuel x = (floor (x/3)) - 2

getFuelDerivative :: Double -> Int
getFuelDerivative x
    | fuel x > 0 = truncate x + getFuelDerivative (fromIntegral (fuel x))
    | fuel x <= 0 = truncate x

main = do
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let modules = words contents
    let intModules = map read (modules) :: [Double]
    let fuels = map fuel intModules
    let fuelDerivatives = map getFuelDerivative (map fromIntegral fuels)
    print(foldr (+) 0  fuelDerivatives)
    return ()

