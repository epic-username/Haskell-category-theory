type KleisliOptional a = (a, Bool)

comp ::  (b -> KleisliOptional c) -> (a -> KleisliOptional b) -> (a -> KleisliOptional c) 
comp f g = helper f g where
    helper :: (b -> KleisliOptional c) -> (a -> KleisliOptional b) -> a -> KleisliOptional c -- notice  a -> KleisliOptional c is converted to (a -> KleisliOptional c) using partial application
    helper f g x = let
        (y, v1) = g x
        (z, v2) = f y
        in
        (z, v1 && v2)

identity :: a -> KleisliOptional a
identity x = (x, True)

safe_reciprocal :: Double -> KleisliOptional Double
safe_reciprocal 0 = (0, False)
safe_reciprocal x = (1 / x, True)

safe_root :: Double -> KleisliOptional Double
safe_root x
    | x < 0 = (0, False)
    | otherwise = (x**0.5, True)

safe_root_reciprocal = safe_root `comp` safe_reciprocal
