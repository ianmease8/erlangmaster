-module(get_facility).

-behaviour(gen_server).

%% API
-export([start_link/0,stop/0,get_info/1]).
% -import(riakc_pb_socket).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%%
%% @spec start -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop() -> gen_server:call(?MODULE, stop).

%%--------------------------------------------------------------------
%% @doc
%% Stops the server gracefully
%% @spec start -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
get_info(Facility_Id)-> gen_server:call(?MODULE, {get_facility,Facility_Id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	riakc_pb_socket:start_link("<your_ip/@>", 8087).
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages. The Request parameter is a tuple
%% consisting of a command, a binary that is the name of the person 
%% for which friends are to be stored, and a binary that is a list 
%% of friends. Both of these binaries can be created using term_to_binary.
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_facility,[]}, _From, Riak_Pid) ->
	{reply,{fail},Riak_Pid};
handle_call({get_facility,Facility_Id}, _From, Riak_Pid) ->
    pass;
handle_call(stop, _From, _State) ->
	{stop,normal,
                server_stopped,
          down}; %% setting the server's internal state to down
handle_call(_,_From, Riak_Pid) ->
	{reply,{fail},Riak_Pid}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
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
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(EUNIT).
  -include_lib("eunit/include/eunit.hrl").

% stop_test() ->
%     ok.

% get_info_test() ->
%     ok.

get_facility_test() ->
    {setup,
        fun() -> meck:new(riakc_pb_socket),
                 meck:expect(riakc_pb_socket, get, fun(riak_pid,"<<facility>>","abc123") -> {ok,"city"} end) end,
        fun() -> meck:unload(riakc_pb_socket) end,
        [
            ?assertMatch({reply,{"city"},riak_pid}, handle_call({get_facility,"abc123"},steve,riak_pid)),
            ?assertMatch({reply,{fail},riak_pid}, handle_call({get_facility,"notPresent"},steve,riak_pid)),
            ?assertMatch({reply,{fail},riak_pid}, handle_call({get_facility,atom},steve,riak_pid)),
            ?assertMatch({reply,{fail},riak_pid}, handle_call({get_facility,123},steve,riak_pid)),
            ?assertMatch({reply,{fail},riak_pid}, handle_call({get_facility,[]},steve,riak_pid)),
            ?assertMatch({reply,{fail},riak_pid}, handle_call({get_facility,[1,2,3]},steve,riak_pid)), % is_string()
            ?assertMatch({reply,{fail},riak_pid}, handle_call({get_facility,[a,b,c]},steve,riak_pid)), 
            ?assertMatch({reply,{fail},riak_pid}, handle_call({get_facility,1.23},steve,riak_pid))
        ]}. 
-endif.

