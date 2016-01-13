%%-*-erlang-*-
{config, 
 session_mgrs, 
 fun(L) when is_list(L)->  
	 F = fun(X) -> 
		     {ok,T} = 
			 inet_parse:address(binary_to_list(X)), T 
	     end,
	 [ F(X) || X <- L]
 end, 
 []
}.
