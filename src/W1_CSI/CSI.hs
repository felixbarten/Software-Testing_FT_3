module W1_CSI.CSI where


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
{-
says :: Boy -> Boy -> Bool
says Matthew b = (says Matthew b && b /= Matthew) && ( says Matthew b && b /= Carl)
says Peter b = (says Peter b && b == Matthew) || (says Peter b && b == Jack)
says Jack b =  (not ((says Matthew b && b /= Matthew) && ( says Matthew b && b /= Carl))) && (not ((says Peter b && b == Matthew) || (says Peter b && b == Jack)))
says Arnold b = ((((says Matthew b && b /= Matthew) && ( says Matthew b && b /= Carl)) && (not ((says Peter b && b == Matthew) || (says Peter b && b == Jack)))) || (not((says Matthew b && b /= Matthew) && ( says Matthew b && b /= Carl)) &&  ((says Peter b && b == Matthew) || (says Peter b && b == Jack))))
says Carl b = not ((((says Matthew b && b /= Matthew) && ( says Matthew b && b /= Carl)) && (not ((says Peter b && b == Matthew) || (says Peter b && b == Jack)))) || (not((says Matthew b && b /= Matthew) && ( says Matthew b && b /= Carl)) &&  ((says Peter b && b == Matthew) || (says Peter b && b == Jack))))

-}

{-
says :: Boy -> Boy -> Bool
says Matthew b = (says Matthew b && b /= Matthew) && ( says Matthew b && b /= Carl)
says Peter b = (says Peter b && b == Matthew) || (says Peter b && b == Jack)
says Jack b = (says Jack b && b == Peter) && (says Jack b && b == Matthew)
says Arnold b = ((says Arnold b && b == Matthew) || (says Arnold b && b == Peter)) && not ((says Arnold b && b == Matthew) && (says Arnold b && b == Peter))
says Carl b = says Carl b && b == Arnold

-}

says :: Boy -> Boy -> Bool
says Matthew b = not ( b == Matthew && b == Carl)
says Peter b = b == Matthew || b == Jack
says Jack b = (says Matthew b) &&  not(says Peter b) 
says Arnold b = ((says Matthew b) && not (says Peter b) || ((not ( says Matthew b)) && (says Peter b)))
says Carl b = not (says Arnold b)

{-
accusers :: Boy -> [Boy]
accusers Matthew = [Peter]
accusers Peter = []
accusers Jack = [Peter]
accusers Arnold = []
accusers Carl = []
-}
accusers :: Boy -> [Boy]
accusers b = filter(\x -> says x b ) boys


honest :: [Boy]
honest = undefined

guilty :: [Boy]
guilty = undefined
