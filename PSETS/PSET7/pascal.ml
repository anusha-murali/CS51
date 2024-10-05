let rec pascal (r:int) (c:int):int = 
	if r =1 || c = 1 then 1 
	else (
              if (r > 1 && c > 1) then
	 	pascal (r-1) c + pascal r (c-1) 
	      else raise Invalid_argument
             )
;;
