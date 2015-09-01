module CSI where


{- 
Exercise CSI:

A group of five school children is caught in a crime. 
One of them has stolen something from some kid they all dislike. 
The headmistress has to find out who did it. 
She questions the children, and this is what they say:

Matthew: Carl didn’t do it, and neither did I.

Peter It was Matthew or it was Jack.

Jack Matthew and Peter are both lying.

Arnold Matthew or Peter is speaking the truth, but not both.

Carl What Arnold says is not true.

Their class teacher now comes in. She says: three of these boys always tell the truth, and two always lie. 
You can assume that what the class teacher says is true. Use Haskell to write a function that computes who was the thief, 
and a function that computes which boys made honest declarations. 
-}

data Boy = Matthew | Peter | Jack | Arnold | Carl 
          deriving (Eq,Show)
 
boys :: [Boy]
boys = [Matthew, Peter, Jack, Arnold, Carl]



says :: Boy -> Boy -> Bool
says = undefined

accusers :: Boy -> [Boy]
accusers = undefined

honest :: [Boy]
honest = undefined

guilty :: [Boy]
guilty = undefined
