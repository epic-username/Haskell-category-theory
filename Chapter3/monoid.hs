class SetSemigroup a where --Semigroup is group without inverse & identity
    operation :: a -> a -> a

class (SetSemigroup a) => SetMonoid a where -- Monoid is group without inverses
    identity :: a

data Z5Star = Z5Star {value:: Integer} deriving (Eq, Ord, Show, Read) -- Z(5)* is {1,...,4} with multiplication operation

instance SetSemigroup Z5Star where
    operation (Z5Star{value = m}) (Z5Star {value = n}) = Z5Star {value = ( n*m `mod` 5)}

instance SetMonoid Z5Star where
    identity = Z5Star{value = 1}
    