-- show isomorphism between a+a and 2a, 2 = Bool
eitherToPair:: Either a a -> (Bool, a)
eitherToPair x = case x of
    Left u -> (True, u)
    Right u -> (False, u)

pairToEither:: (Bool, a) -> Either a a
pairToEither (True, x)  = Left x
pairToEither (False, x) = Right x
