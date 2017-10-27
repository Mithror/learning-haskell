module Vehicle where

    data Price = Price Integer deriving (Eq, Show)
    data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
    data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited
        deriving (Eq, Show)

    data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

    myCar :: Vehicle
    myCar = Car Mini (Price 14000)
    
    urCar :: Vehicle
    urCar = Car Mazda (Price 20000)

    clownCar :: Vehicle
    clownCar = Car Tata (Price 7000)

    doge :: Vehicle
    doge = Plane PapuAir

    -- 1
    -- myCar :: Vehicle

    -- 2
    isCar :: Vehicle -> Bool
    isCar (Car _ _) = True
    isCar _         = False

    isPlane :: Vehicle -> Bool
    isPlane (Plane _) = True
    isPlane _         = False

    areCars :: [Vehicle] -> [Bool]
    areCars = map isCar

    -- 3
    getManu :: Vehicle -> Manufacturer
    getManu (Car m _) = m
    getManu _         = undefined

    -- 4
    -- Would generate a partial function error.
    -- You could change it to a Maybe Manufacturer and return Nothing

    -- 5
    data Size = Size Integer deriving (Eq, Show)
    data Vehicle' = Car' Manufacturer | Plane' Airline Size deriving (Eq, Show)
    doge' :: Vehicle'
    doge' = Plane' PapuAir (Size 120)
    -- isCar stays the same
    isPlane' :: Vehicle' -> Bool
    isPlane' (Plane' _ _) = True
    isPlane' _            = False
    -- areCars stays the same
    -- getManu stays the same