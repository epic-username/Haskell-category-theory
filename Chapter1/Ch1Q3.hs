
identity:: a -> a
identity x = x
comp:: (b->c) -> (a->b) -> a -> c
comp f g x = (f.g) x

leftId:: (a->b) -> a -> b
leftId f x = comp identity f x

rightId:: (a->b) -> a -> b
rightId f x = comp f identity x