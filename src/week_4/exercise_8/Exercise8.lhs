> module Exercise8 where

Is there a difference between the symmetric closure of the transitive closure of a relation R and the transitive closure of the symmetric closure of R?

Yes, the transitive closure of a symmetric closure of a non-empty relation will always produce a reflexive relation but not the other way around.

For example:

    Exercise7> let r = [(1,2), (2,2), (2,3)]
    
    Exercise7> trClos $ symClos r
    
    [(1,2),(2,1),(2,2),(2,3),(3,2),(1,1),(1,3),(3,1),(3,3)] *-> This relation is reflexive*
    
    Exercise7> symClos $ trClos r
    
    [(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2)] *-> This one is not*

