%% @author Lee Barney
%% @copyright 2022 Lee Barney licensed under the <a>
%%        rel="license"
%%        href="http://creativecommons.org/licenses/by/4.0/"
%%        target="_blank">
%%        Creative Commons Attribution 4.0 International License</a>
%%
%%
-module(fancy_math).
-behaviour(gen_server).
-define(SERVER,?MODULE).
%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
% -export([start/0,start/3,stop/0,add/1,remove/1,remove_all/0,see_enrollment/0]).
-export([start/0,start/2,start/3,stop/0,power/2,mod/2,factorial/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
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
-spec start(atom(),atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Name,Registration_type,Args) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, Args, []).

-spec start(atom(),atom()) -> {ok, pid()} | ignore | {error, term()}.
start(Registration_type,Name) ->
    gen_server:start_link({Registration_type, Name}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> {ok}|{error, term()}.
stop() -> gen_server:call(?MODULE, stop).

%% Any other API functions go here.

power(Num1,Num2) ->
    gen_server:call(?MODULE,{square,Num1,Num2}).

factorial(Num) ->
    gen_server:call(?MODULE,{factorial,Num}).

mod(Num1,Num2) ->
    gen_server:call(?MODULE,{mod,Num1,Num2}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%--------------------------------------------------------------------
-spec init(term()) -> {ok, term()}|{ok, term(), number()}|ignore |{stop, term()}.
init([]) ->
        {ok,replace_up}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request::term(), From::pid(), State::term()) ->
                                  {reply, term(), term()} |
                                  {reply, term(), term(), integer()} |
                                  {noreply, term()} |
                                  {noreply, term(), integer()} |
                                  {stop, term(), term(), integer()} | 
                                  {stop, term(), term()}.
% handle_call(_Request, _From, State) ->
%         {reply,replace_started,State};
handle_call(stop, _From, _State) ->
        {stop,normal,
                replace_stopped,
          down}; %% setting the server's internal state to down

handle_call({power,Num1,Num2},_From,State) when is_integer(Num1), is_integer(Num2) ->
    {reply,
    math:pow(Num1,Num2),
    State};

handle_call({factorial,Num},_From,State) when is_integer(Num), Num >= 0 ->
    {reply,
    fact(Num),
    State};

handle_call({mod,Num1,Num2},_From,State) when is_integer(Num1), is_integer(Num2) ->
    {reply,
    Num1 rem Num2,
    State}.

fact(0) -> 1;
fact(N) -> N * fact(N-1).
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



-ifdef(EUNIT).
% -define(DEBUG).
%%
%% Unit tests go here. 
%%


% startup_test_() ->
%     {setup,
%         fun() -> ok end,
%         fun(_) -> gen_server:stop(?MODULE),
%                  gen_server:stop(bill) end,
%         [?_assertMatch({ok,_},start()),
%          ?_assertMatch({ok,_},start(local,bill))]}.

% see_enrollment test

% _test() ->
%     {setup,
%         fun() -> gen_server:start({local,action},[],[]) end,
%         fun() -> gen_server:stop(action) end,
%         [
%             ?assertMatch({reply,answer,[]}, handle_call({action,Num1,Num2},nil,[])),
%             ?assertException(error, function_clause, handle_call({action,item,item},nil,[])),
            
%         ]}.

power_test() ->
    {setup,
        fun() -> gen_server:start({local,power_action},[],[]) end,
        fun() -> gen_server:stop(power_action) end,
        [
            ?assertMatch({reply,1.0,[]}, handle_call({power,0,0},nil,[])),
            ?assertMatch({reply,1.0,[]}, handle_call({power,1,1},nil,[])),
            ?assertMatch({reply,-1.0,[]}, handle_call({power,-1,1},nil,[])),
            ?assertException(error, function_clause, handle_call({power,"list",1},nil,[])),
            ?assertException(error, function_clause, handle_call({power,1.5,1},nil,[]))
        ]}.

factorial_test() ->
    {setup,
        fun() -> gen_server:start({local,factorial_action},[],[]) end,
        fun() -> gen_server:stop(factorial_action) end,
        [
            ?assertMatch({reply,1,[]}, handle_call({factorial,0},nil,[])),
            ?assertMatch({reply,120,[]}, handle_call({factorial,5},nil,[])),
            ?assertException(error, function_clause, handle_call({factorial,-1},nil,[])),
            ?assertException(error, function_clause, handle_call({factorial,1.5},nil,[])),
            ?assertException(error, function_clause, handle_call({factorial,2.5},nil,[]))
        ]}.

mod_test() ->
    {setup,
        fun() -> gen_server:start({local,mod_action},[],[]) end,
        fun() -> gen_server:stop(mod_action) end,
        [
            ?assertMatch({reply,0,[]}, handle_call({mod,1,1},nil,[])),
            ?assertMatch({reply,-1,[]}, handle_call({mod,-1,2},nil,[])),
            ?assertMatch({reply,1,[]}, handle_call({mod,21,5},nil,[])),
            ?assertException(error, function_clause, handle_call({mod,1.5,1},nil,[])),
            ?assertException(error, function_clause, handle_call({mod,"list",1},nil,[])),
            ?assertException(error, badarith, handle_call({mod, 17,0},nil,[]))
        ]}.

% enrollment_test() -> 
%     {setup,
%         fun() -> gen_server:start({local,enrollment_name},[],[]) end,
%         fun() -> gen_server:stop(enrollment_name) end,
%         [?assertMatch({reply,{ok,[]},[]},handle_call(see_enrollment,nil,[])),
%         ?assertMatch({reply,{ok,[bob,sue,frank]},[bob,sue,frank]},handle_call(see_enrollment,nil,[bob,sue,frank]))]
%     }.

% % add test
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
%     ]}.

% % remove all test
% remove_all_test() -> 
%     {setup,
%         fun() -> gen_server:start({local,remove_name},[],[]) end,
%         fun() -> gen_server:stop(remove_name) end,
%         [
%             ?assertMatch({reply,ok,[]},handle_call(remove_all,nil,[steve,bill])),
%             ?assertMatch({reply,ok,[]},handle_call(remove_all,nil,[]))
%     ]}.
% remove all test
% remove_
% handle_call({remove,Student},_From,Class) ->
%     {reply,
%         ok,
%         lists:delete(Student,Class)};



-endif.
