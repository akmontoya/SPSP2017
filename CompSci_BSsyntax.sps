
regression /dep = interest /method = enter cond. 
regression /dep = CScomm /method = enter cond. 
regression /dep = interest /method = enter cond CScomm. 

PROCESS vars = cond interest CScomm /x = cond /m = CScomm /y = interest / model = 4 /total = 1/percent = 1 /save = 1. 

descriptives grppref. 

regression /dep = interest /method = enter cond grppref. 

compute condxgrppref = cond * grppref. 
execute. 
regression /dep = interest /method = enter cond grppref condxgrppref. 


PROCESS vars = cond interest grppref /x = cond /m = grppref /y = interest / model = 1 /jn = 1 /plot = 1. 

