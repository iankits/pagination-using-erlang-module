%%%-------------------------------------------------------------------
%%% @author     Ankit Singh (ankit@hover.in)
%%% @copyright  2008 hover.in
%%% @doc  Module to handle all pagination of any List produced by any
%%%       M:F(A) and for each item to be handle by output handler 
%%%       function and show appropriate page Navigation.
%%% @reference hi_cache_worker
%%% @end
%%%-------------------------------------------------------------------
-module(hi_pagination_worker).

-behaviour(gen_server).

%% Includes
-include("hi_definitions.hrl").

%% API
-export([start/0,paginate/5, handle_content/3, handle_navigation/4
	]).

%% supervision
-export([start_link/0,start_link/1,get_child_spec/1]).

%% gen_server callbacks
-export([
 	 init/1, handle_call/3, handle_cast/2, handle_info/2,
 	 terminate/2, code_change/3,group_name/0
 ]).

-define(SERVER,?MODULE).
-define(WORKER_PREFIX,"hi_pagination_worker_").
-define(HI_PG_PAGINATE, "hi_pg_paginate").
%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link()->
    ?MODULE:start_link({list_to_atom(?WORKER_PREFIX)}).

start_link({WorkerId})->
    ?MODULE:start_link({WorkerId, ?MODULE:group_name()});

start_link({WorkerId,_PG}=InitArg) ->
    io:format("~nin pagination_worker startlink/1: InitArg = ~p",[InitArg]),
    gen_server:start_link({local, WorkerId}, ?MODULE, InitArg,[]).

%%--------------------------------------------------------------------
%% Function: get_child_spec
%% Description: @doc
%% The worker restart strategy Logic in a function in the same module ,
%% so that any supervisor can take on responsibility of this worker,
%% by just invoking this function for the spec.
%% @end
%%--------------------------------------------------------------------
get_child_spec(NoOfWorkers)->
    Workers = lists:map(fun( X )->
				WorkerId = list_to_atom(?WORKER_PREFIX++ integer_to_list(X)),
				{WorkerId, {?MODULE, start_link, [{WorkerId}] },
            permanent, 10, worker, [?MODULE]}		     
    end, lists:seq(1,NoOfWorkers)),
    Workers.
   
    
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(_Arg) ->
    process_flag(trap_exit, true),
    ProcessGroup = ?MODULE:group_name(),
    io:format("~n Init:  PG:~p~n",[ProcessGroup]),
    ok=pg2:create(ProcessGroup),
    ok=pg2:join(ProcessGroup,self()),
    {ok,[]}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({paginate, FList, HandlerFun, OptQuery, Page, MaxItemsPerPage_},_From,_State)->
    case FList of
	[] ->
	    %error_logger:info_msg("pagination worker: flist empty !!!!!!!",[]),
	    Reply = [ [], [] ];
	List ->
	    Length = length(List),
	    
	    CurrPage = normalizeString(Page,1),
	    
	    MaxItemsPerPage = normalizeString(MaxItemsPerPage_,10),
	    
	    NumPages  = findTotalPages(Length, MaxItemsPerPage),
	    %io:format("~n NumPages -  : ~p CurrPage = ~p",[NumPages,CurrPage]),
	    %% if Current Page is not valid or CurrPage > NumPages is not valid, redirect to last page
	    Start = if
			CurrPage =< NumPages ->
			    _Start = (  (CurrPage-1) * MaxItemsPerPage) + 1,
			    _Start;
			%% if Currpages > NumPages then redirect to last page or any other Pages.
			CurrPage > NumPages ->
			    %io:format("~nhi I am here ~n ",[]),
			    _Start = (  (NumPages-1) * MaxItemsPerPage) + 1,
			    _Start
		    
		    end,
	    %io:format("~n Handle Call - start : ~p",[Start]),
	    FilteredList = lists:sublist(List, Start, MaxItemsPerPage),
	    
	    Reply = [ ?MODULE:handle_content(Length, FilteredList, HandlerFun ),
		      ?MODULE:handle_navigation( OptQuery, CurrPage, MaxItemsPerPage,NumPages) 
		     ]
    
    end,
	    %%io:format("~n Reply ~p",[Reply]),
	    {reply,Reply,_State};
	
handle_call(_Request,_From,State)->
    Reply = [],
    {reply,Reply,State}.
  
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Request,State)->
    io:format("~n Unexpected cast request",[]),
    {noreply,State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', _Arg, _Reason}, State) ->
    io:format("~n EXIT caught in worker: ~p",[_Reason]),
    %%{stop, { _Arg,_Reason}, State};
    {noreply,State};

handle_info(_Info, State) ->
    {noreply, State}.
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format("~n Termination to due to REASON:~p",[_Reason]),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% API functions
%%--------------------------------------------------------------------
paginate(List,ContentHandlerFun,OptQuery,Page,MaxItemsPerPage)->
    %io:format("~n ~p, paginate",[?MODULE]),
    ProcessGroup = ?MODULE:group_name(),
    Pid = pg2:get_closest_pid(ProcessGroup),
    %io:format("~nsend to closest ~p group -> ~p",[ProcessGroup,Pid]),

    R = gen_server:call(Pid,{paginate,List,ContentHandlerFun, OptQuery,Page, MaxItemsPerPage }),
    R.

%% This function takes care of Data Retrievation
handle_content(_Length, AllList, HandlerFun) ->
    Content = lists:map ( fun(X) -> HandlerFun(X) end , AllList),
    Content.

%% This function takes care of Pagination
handle_navigation(OptQuery, TempCurrPage, MaxItems, NumPages) ->
    MaxSubPage = 10,
    %% if Current Page is not valid or CurrPage > NumPages is not valid, redirect to last page
    {StartPage,EndPage,CurrPage}= case NumPages of 
				      Pages when TempCurrPage =< Pages ->
					  _StartPage = startPageFrom( TempCurrPage , MaxSubPage ),
					  _EndPage = endPageTo(_StartPage,TempCurrPage, Pages, MaxSubPage),
					  {_StartPage, _EndPage,TempCurrPage};
				      %% if Currpages > NumPages then redirect to last page or any other Pages.
				      Pages when TempCurrPage > Pages ->
					  _StartPage = startPageFrom( Pages , MaxSubPage ),
					  _EndPage = endPageTo(_StartPage,Pages,Pages, MaxSubPage),
					  _CurrPage = NumPages,
					  {_StartPage, _EndPage,_CurrPage}
				  end,
    
    %%io:format("~n StartSubPage=~p EndSubPage=~p,currpage = ~p",[StartPage,EndPage,CurrPage]),
    
    %% if there is only one Page, then Active & Previous Page will be OFF
    case NumPages of
	1 -> 
	    ReturnHTML=noActiveLink(OptQuery,MaxItems,CurrPage,StartPage,EndPage);
	_->	
	    if
		CurrPage == 1 ->
		     ReturnHTML=activePageEqOne(OptQuery,MaxItems,CurrPage,StartPage,EndPage);
		
		CurrPage == NumPages ->
		    
		     ReturnHTML=activePageEqMaxPage(OptQuery,MaxItems,CurrPage,StartPage,EndPage);
		
		CurrPage < NumPages ->
		     ReturnHTML=activepageGrtMaxPage(OptQuery,MaxItems,CurrPage,StartPage,EndPage)
	    end
    end,
    ReturnHTML.

startPageFrom(ActivePage,MaxSubPerPage) ->
    FinalSubPage = findTotalPages(ActivePage,MaxSubPerPage),
    StartSubPage = FinalSubPage * 10 + 1 - 10, %% Here if 0 -> 1, 1 -> 2.
    StartSubPage.

endPageTo(StartSubPage,ActivePage, NumPages, MaxSubPage) ->
    SubPage = ActivePage / MaxSubPage,
    TempEndPage = StartSubPage + 10 - 1,
    
    %% if TempEndPage > NumPages then EndPage = NumPages
    if 
	SubPage >= 1 ->
	    EndSubPage = if
			     TempEndPage < NumPages ->
				 TempEndPage;
			     TempEndPage > NumPages ->
				 _TempEndPage = NumPages,
				 _TempEndPage
			 end;
	SubPage < 1 ->
	    StartSubPage = 1,
	    %% Check if there is only one SubPage, then End page will be < 10 
	    EndSubPage = if
			     TempEndPage < NumPages ->
				 _TempEndPage = 10,
				 _TempEndPage;
			     TempEndPage > NumPages ->
				 _TempEndPage = NumPages,
				 _TempEndPage
			 end
    
    end,
    EndSubPage.

%%for standalone without supervisor, useful when debugging
start()->
    ?MODULE:start_link().

group_name()->
    ?HI_PG_PAGINATE ++ "_"++atom_to_list( node()).

findTotalPages(TotalLength,Max)->
    DivBy = TotalLength / Max,
    RoundPages = round(DivBy),
    %% refine RoundPages as for less than .4, still be need one more page
    %% 2.4 = 2 but we need 3. 
    RemCheck = RoundPages - DivBy,
    ReturnPages = if  
		      RemCheck == 0 ->
			  RoundPages;
		      RemCheck =< 0 ->
			  _FPages = RoundPages + 1,
			  _FPages;
		      RemCheck > 0 ->
			  RoundPages
		  end,
    ReturnPages.

%% if there is only one Page, then Active & Previous Page will be OFF
noActiveLink(OptQuery,NormDispBy,ActivePage,StartSubPage,EndSubPage) ->
    %%io:format("~n NumPages == 1",[]),
    
    Body1=bodyPrevOff(),
    Body2=bodyActivePage(OptQuery,ActivePage,NormDispBy,StartSubPage,EndSubPage),
    Body3=bodyNxtOff(),
    HTML = Body1++Body2++Body3,
    
    HTML.

%% When Active page == 1, then PREVIOUS Button will be off
activePageEqOne(OptQuery,NormDispBy,ActivePage,StartSubPage,EndSubPage) ->
    %%io:format("~n ActivePage == 1",[]),
   
    Body1=bodyPrevOff(),
    Body2=bodyActivePage(OptQuery,ActivePage,NormDispBy,StartSubPage,EndSubPage),
    Body3=bodyNxtOn(OptQuery,ActivePage,NormDispBy),
    HTML = Body1++Body2++Body3,
    
    HTML.

%% When Active page = Max Number of Pages, then NEXT Button will be off
activePageEqMaxPage(OptQuery,NormDispBy,ActivePage,StartSubPage,EndSubPage) ->
    %%io:format("~n ActivePage = NumPages",[]),
   
    Body1=bodyPrevOn(OptQuery,ActivePage,NormDispBy),
    Body2=bodyActivePage(OptQuery,ActivePage,NormDispBy,StartSubPage,EndSubPage),
    Body3=bodyNxtOff(),
    HTML = Body1++Body2++Body3,
    HTML.

%% When Active page < Max Number of Pages, then PREVIOUS and NEXT Button will be active
activepageGrtMaxPage(OptQuery,NormDispBy,ActivePage,StartSubPage,EndSubPage) ->
    %%io:format("~n ActivePage < NumPages",[]),
   
    Body1=bodyPrevOn(OptQuery,ActivePage,NormDispBy),
    Body2=bodyActivePage(OptQuery,ActivePage,NormDispBy,StartSubPage,EndSubPage),
    Body3=bodyNxtOn(OptQuery,ActivePage,NormDispBy),
    HTML = Body1++Body2++Body3,
    HTML.

bodyPrevOff()->
    Body1= "<div class='paginatorDiv'><ul id=\"pagination-hover\"><li class=\"previous-off\"> << Previous </li>",
    Body1.

bodyPrevOn(OptQuery,ActivePageInt,NormDispBy) ->
    Body1="<div class='paginatorDiv'><ul id=\"pagination-hover\"><li class=\"previous\"><a href=\"?"++OptQuery++"&max="++normalizeToList(NormDispBy)++"&page="++normalizeToList(ActivePageInt-1)++"\"> << Previous </a></li>" ,
    Body1.

bodyActivePage(OptQuery,ActivePageInt,NormDispBy,StartSubPage,EndSubPage) ->
    Body2=lists:foldl(fun(Page,Prev) -> 
			    Class= case Page of 
				       ActivePageInt ->
					   "active";
				       _ ->
					   " "
				   end,
			   Prev ++ "<li><a class=\""++ Class ++"\" href=\"?"++OptQuery++"max="++normalizeToList(NormDispBy)++"&page="++normalizeToList(Page)++"\">"++normalizeToList(Page)++"</a></li>" 
		    end,"",lists:seq(StartSubPage,EndSubPage)),
    Body2.

bodyNxtOff() ->
    Body3="<ul id=\"pagination-hover\"><li class=\"next-off\">  Next >></li></ul></div>",
    Body3.

bodyNxtOn(OptQuery,ActivePage,NormDispBy) ->
    Body3="<li class=\"next\"><a href=\"?"++OptQuery++"max="++normalizeToList(NormDispBy)++"&page="++normalizeToList((ActivePage)+1)++"\"> Next >></a></li></ul></div>",
    Body3.

%%-----------------------------------------------------------------------------
%%Normalize Functions
%%-----------------------------------------------------------------------------
normalizeToList(Attrib) ->
    NormList = integer_to_list(Attrib),
    NormList.

normalizeString(Input,DefaultValue) ->
    R = case string:to_integer(Input) of
	    {error,no_integer} ->
		DefaultValue;
	    {Int,[]} ->
		Int;
	    _-> 
		DefaultValue
	end,
    
    R.
