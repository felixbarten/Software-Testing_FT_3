data Boy = Matthew | Peter | Jack | Arnold | Carl 
		deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

says :: 	Boy -> Boy -> Bool
says Matthew x = not(x == Carl) && not(x == Matthew)
says Peter x = x == Matthew || x == Jack
says Jack x = not(says Matthew x ) && not(says Peter x)
says Arnold x = ((not (says Matthew x)) || (says Peter x)) && not ((not (says Matthew x)) && (says Peter x))
says Carl x = not(says Arnold x)

accusers :: Boy -> [Boy]
accusers x = filter(\b -> says b x) boys

guilty :: [Boy]
guilty = undefined