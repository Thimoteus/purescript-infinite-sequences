#purescript-infinite-sequences

An infinite list/array/sequence can be thought of as a function from its indices
to its values: `newtype Sequence a = Sequence (Int -> a)`.

Of course, PS's `Int` type is bounded, but we treat it as if it were not.