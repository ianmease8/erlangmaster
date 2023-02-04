%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(rr_state_machine).
-behaviour(gen_statem).
-define(SERVER,?MODULE).
%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
% -export([start/0,start/3,stop/0,add/1,remove/1,remove_all/0,see_enrollment/0]).
-export([start/0,start/2,start/3,stop/0,add/1,remove/1,get_next/0,start_link/1,start_link/2,start_link/3]).

%% gen_statem callbacks
-export([init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% State Callbacks
-export([handle_event/4]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server assuming there is only one server started for 
%% this module. The server is registered locally with the registered
%% name being the name of the module.
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()} | ignore | {error, term()}.
start() ->
    gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts a server using this module and registers the server using
%% the name given.
%% Registration_type can be local or global.
%%
%% Args is a list containing any data to be passed to the gen_server's
%% init function.
%%
%% @end
%%--------------------------------------------------------------------
% -spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).

-spec start(atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name) ->
    gen_statem:start_link({Registration_type, Name}, ?MODULE, [], []).

-spec start_link(atom(),term(),term()) -> {ok, atom()}.
start_link(Statem_name,Initial_state,Args) ->
    gen_statem:start_link({local,Statem_name},?MODULE,Initial_state,[]).

-spec start_link(atom(),term()) -> {ok, atom()}.
start_link(Statem_name,Initial_state) ->
    gen_statem:start_link({local,Statem_name},?MODULE,Initial_state,[]).

-spec start_link(atom()) -> {ok, atom(), tuple()}.
start_link(Statem_name) ->
    gen_statem:start_link({local,Statem_name},?MODULE,[],[]).
%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

% Our API functions
add(New_egg) -> gen_statem:call(rr_state_machine,{add,New_egg}).

remove(Egg) -> gen_statem:call(?MODULE,{remove,Egg}).

get_next() -> gen_statem:call(?MODULE,get_next).
%% Any other API functions go here.

% add(Name) ->
%     gen_server:call(?MODULE,{add,Name}).

% remove(Name) ->
%     gen_server:call(?MODULE,{remove,Name}).

% get_next() ->
%     gen_server:call(?MODULE,get_next).
    


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%---------------------------------------------------------------------

% init([H|T]) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
%     {ok,H,{name,T++[H]}};
% init([]) ->
%     {ok,[],{name,[]}};
% init(Args) ->
%     {ok,[],{name,[]}}.
% -spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
        {ok,[]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle events 
%%
%% @end
%%--------------------------------------------------------------------

handle_event({call,From}, {add,Egg}, [], {_Statem_name,Nest}) when is_atom(Egg)->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, Egg,{Egg,Nest++[Egg]},[{reply,From,egg_added}]};


handle_event({call,From}, {add,Egg}, State, {_Statem_name,Nest}) when is_atom(Egg) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, State,{State,Nest++[Egg]},[{reply,From,egg_added}]};

handle_event({call,From}, {remove,Egg}, Egg, {_Statem_name,[Egg]}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, nil,{nil,[]},[{reply,From,egg_removed}]};

handle_event({call,From}, {remove,Egg}, State, {_Statem_name,Nest}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, State,{State,lists:delete(Egg,Nest)},[{reply,From,egg_removed}]};

% [H|T] == [Egg|Nest]
handle_event({call,From}, get_next, State, {_Statem_name,[Egg|Nest]}) -> 
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, Egg,{Egg,Nest++[Egg]},[{reply,From,State}]}.




%         {reply,replace_started,State};
% handle_call(stop, _From, _State) ->
%         {stop,normal,
%                 replace_stopped,
%           down}; %% setting the server's internal state to down
% handle_call({add,Process},_From,List_of_pids) ->
%     % io:debugFmt("adding ~p to ~p to get ~p~n", [Student,Class,[Student]++Class]),
%     {reply,
%         ok,
%         [Process]++List_of_pids};
% handle_call({remove,Process},_From,List_of_pids) ->
%     {reply,
%         ok,
%         lists:delete(Process,List_of_pids)};

% handle_call(get_next,_From,[H|T]) ->
%     {reply,
%         H,
%         T++[H]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Msg::term(), State::term()) -> {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @end
-spec handle_info(Info::term(), State::term()) -> {noreply, term()} |
                                   {noreply, term(), integer()} |
                                   {stop, term(), term()}.
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason::term(), term()) -> term().
terminate(_Reason, _State) ->
    ok.
    
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), term(), term()) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================






%%--------------------------------------------------------------------
%% @doc
%% This function gracefully shuts down the balancer.
%%
%% The parameter of stop is an atom that
%% is a registered name of a round robin balancer.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(atom()) -> ok.
stop(Statem_name) ->
    gen_statem:stop(Statem_name).

%% Mandatory callback functions
%% @private
terminate(_Reason, _State, _Data) ->
    void.
%% @private
code_change(_Vsn, State, Data, _Extra) ->
    {ok,State,Data}.
%% @private

    % {ok,ready,Worker_ids}.
%% @private
callback_mode() -> handle_event_function.

%%% state callback(s)


%%
%% Used to select which registered worker is to be used next in 
%% a round robin fashion.
%% @private
% handle_event({call,From}, next, ready,{Statem_name,State_data}) ->
%     %Modify the state data and replace State_data below with the modified state data.
%     {next_state, ready,{Statem_name,State_data},[{reply,From,Statem_name}]}.

% handle_event({call,From}, down, {up,computer_side,open}, _) ->




-ifdef(EUNIT).
% -define(DEBUG).
%%
%% Unit tests go here. 
%%
add_test() ->
    {setup,
        fun() -> gen_statem:start_link({local,add_egg},?MODULE,[],[]) end,
        fun() -> gen_statem:stop(add_egg) end,
        [  
            ?assertMatch({next_state, steve,{steve,[steve]},[{reply,batman,egg_added}]}, handle_event({call,batman},{add,steve},[],{add_egg,[]})),
            ?assertMatch({next_state, steve,{steve,[steve,hannah]},[{reply,batman,egg_added}]}, handle_event({call,batman},{add,hannah},steve,{add_egg,[steve]})),
            ?assertException(error, function_clause, handle_event({call,batman},{add,1},steve,{add_egg,[steve]}))
        ]}.

remove_test() ->
    {setup,
        fun() -> gen_statem:start_link({local,remove_egg},?MODULE,[],[]) end,
        fun() -> gen_statem:stop(remove_egg) end,
        [  
            ?assertMatch({next_state, steve,{steve,[steve]},[{reply,batman,egg_removed}]}, handle_event({call,batman},{remove,hannah},steve,{remove_egg,[steve,hannah]})),
            ?assertMatch({next_state, [],{[],[]},[{reply,batman,egg_removed}]}, handle_event({call,batman},{remove,steve},[],{remove_egg,[steve]})),
            ?assertMatch({next_state, steve,{steve,[steve]},[{reply,batman,egg_removed}]}, handle_event({call,batman},{remove,bill},steve,{remove_egg,[steve]})),
            ?assertMatch({next_state, steve,{steve,[steve]},[{reply,batman,egg_removed}]}, handle_event({call,batman},{remove,1},steve,{remove_egg,[steve]})),
            ?assertMatch({next_state, nil,{nil,[]},[{reply,batman,egg_removed}]}, handle_event({call,batman},{remove,bill},nil,{remove_egg,[]}))
        ]}.

get_next_test() ->
    {setup,
        fun() -> gen_statem:start_link({local,get_next_egg},?MODULE,[],[]) end,
        fun() -> gen_statem:stop(get_next_egg) end,
        [  
            ?assertMatch({next_state, steve,{steve,[steve]},[{reply,batman,steve}]}, handle_event({call,batman},get_next,steve,{get_next_egg,[steve]})),
            ?assertMatch({next_state, steve,{steve,[hannah,steve]},[{reply,batman,steve}]}, handle_event({call,batman},get_next,steve,{get_next_egg,[steve,hannah]})),
            ?assertException(error, function_clause, handle_event({call,batman},get_next,[],{get_next_egg,[]}))
        ]}.
% handle_event({call,From}, get_next, State, {_Statem_name,[Egg|Nest]}) -> 
% {next_state, Egg,{Egg,Nest++[Egg]},[{reply,From,State}]}

% startup_test_() ->
%     {setup,
%         fun() -> ok end,
%         fun(_) -> gen_server:stop(?MODULE),
%                  gen_server:stop(bill) end,
%         [?_assertMatch({ok,_},start()),
%          ?_assertMatch({ok,_},start(local,bill))]}.

% add test
% add_test() -> 
%     {setup,
%         fun() -> gen_server:start({local,add_name},[],[]) end,
%         fun() -> gen_server:stop(add_name) end,
%         [?assertMatch({reply,ok,[bill]},handle_call({add, bill},nil,[])),
%         ?assertMatch({reply,ok,[steve,bill]},handle_call({add, steve},nil,[bill])),
%         ?assertException(error, function_clause, handle_call({add,"atom"},nil,[])),
%         ?assertException(error, function_clause, handle_call({add,1},nil,[])),
%         ?assertException(error, function_clause, handle_call({add},nil,[]))
%     ]}.

% % remove test
% remove_test() -> 
%     {setup,
%         fun() -> gen_server:start({local,remove_name},[],[]) end,
%         fun() -> gen_server:stop(remove_name) end,
%         [
%         ?assertMatch({reply,ok,[bill]},handle_call({remove, steve},nil,[steve,bill])),
%         ?assertMatch({reply,ok,[frank]},handle_call({remove, bill},nil,[frank])),
%         ?assertException(error, function_clause, handle_call({remove, 1},nil,[steve,bill])),
%         ?assertException(error, function_clause, handle_call({remove, "atom"},nil,[steve,bill])),
%         ?assertException(error, function_clause, handle_call({remove, []},nil,[steve,bill])),
%         ?assertException(error, function_clause, handle_call({remove, [steve,bill]},nil,[steve,bill]))
%      ]}.

% get_next_test() ->
%     {setup,
%         fun() -> gen_server:start({local,get_next_thing},[],[]) end,
%         fun() -> gen_server:stop(get_next_thing) end,
%         [
%             ?assertMatch({reply,steve,[joe,phil,steve]},handle_call(get_next,nil,[steve,joe,phil])),
%             ?assertMatch({reply,steve,[joe,phil,bob,steve]},handle_call(get_next,nil,[steve,joe,phil,bob]))
%     ]}.
% remove all test
% remove_
% handle_call({remove,Student},_From,Class) ->
%     {reply,
%         ok,
%         lists:delete(Student,Class)};



-endif.