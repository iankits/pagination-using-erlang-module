%%%-------------------------------------------------------------------
%%% @author     Bhasker V Kode <kode@hover.in> Ankit Singh <ankit@hover2>
%%% @copyright  2008 hover.in
%%% @doc supervisor for all hi_pagination_worker childs.Will restart
%%% a pagination related workers when it goes down, provided userlist is passed
%%% into it via init . When restarted - has only information of previously
%%% started workers.  Hence use start_child if new clients as and when
%%% users added, but on crash load upto date user list.
%%% TODO:make this itself supervised
%%% @reference hi_pagination_worker
%%% @end
%%%-------------------------------------------------------------------
-module(hi_pagination_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_link/1,start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WORKER_PREFIX,"hi_pagination_worker_").

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link()->
    start_link([]).

start_link(_Args) ->
    io:format("~nIn pagination_sup:start_link, args from pagination_app is ~p",[_Args]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, _Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init(_Arg) ->

    io:format("~nIn hi_pagination_sup:init, args from pagination_sup:start_link is ~p",[_Arg]),
   
    NUserWorkers =
    
       lists:flatten(
	hi_pagination_worker:get_child_spec(1)
       )
 
    ,
   io:format("~n NuserWorkers = ~p~n",[NUserWorkers]),
   R1 = {ok, {{ one_for_all, 3, 10},
             NUserWorkers
	  }
    },
    R1.


start_child(Config)->
   {ok, {_,ChildSpecs}} = ?MODULE:init(Config),
   [ supervisor:start_child(?MODULE, ChildSpec) || ChildSpec <- ChildSpecs].

%%====================================================================
%% Internal functions
%%====================================================================

