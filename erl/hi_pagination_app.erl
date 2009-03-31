-module(hi_pagination_app).

-behavior(application).

%% application callbacks
-author('ankit@hover.in').

-export([start/2, 
         stop/1]).

start(_Type, _Args) ->
  io:format("~nIn hi_pagination_app start : ~p,~p",[_Type,_Args]),
  case hi_pagination_sup:start_link(_Args) of
	{ok, Pid} -> 
	  {ok,Pid};
	Error ->
	   io:format("~n hi_pagination_app unexpected:~p",[Error]),
	   ok
    end.

stop(_State) ->
    ok.
        
