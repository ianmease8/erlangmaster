%%%-------------------------------------------------------------------
%%% @author Lee Barney  <barney.cit@gmail.com>
%%% @copyright © 2022, Lee S. Barney
%%% @reference Licensed under the 
%%% <a href="http://creativecommons.org/licenses/by/4.0/">
%%% Creative Commons Attribution 4.0 International License</a>.
%%%
%%% @doc
%%% This is a round robin balancer. Given a set of module-id pairs, this balancer
%%% will distribute work in a  
%%% <a href="https://www.techtarget.com/whatis/definition/round-robin">
%%% round-robin</a> fashion.
%%%
%%% To use this round robin balancer, the balanced worker item must have a
%%% locally or globally registered name. The registered name is used 
%%% to add the item to a balancer.
%%%
%%%
%%%
%%% Be aware that a worker item can, via its ID, be added to more than 
%%% one rr_balancer. This is by design, not by accident. 
%%% @end

%%% Created : 24 June 2022 by Lee Barney <barney.cit@gmail.com>
%%%-------------------------------------------------------------------
-module(robot).
-behaviour(gen_statem).

%% Only include the eunit testing library
%% in the compiled code if testing is 
%% being done.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([start/2,start_link/1,start_link/2,stop/1,down/1,up/1,open/1,close/1,go_to_box_side/1,go_to_computer_side/1]).

%% Supervisor Callbacks
-export([terminate/3,code_change/4,init/1,callback_mode/0]).
%% State Callbacks
-export([handle_event/4]).


%%%===================================================================
%%% Public API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%%
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start(atom(),term()) -> {ok, atom()}.
start(Statem_name,Initial_state) ->
    gen_statem:start({local,Statem_name}, ?MODULE, Initial_state, []).

%%--------------------------------------------------------------------
%% @doc
%% 
%% Documentation goes here.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(),term()) -> {ok, atom()}.
start_link(Statem_name,Initial_state) ->
    gen_statem:start_link({local,Statem_name},?MODULE,Initial_state,[]).

-spec start_link(atom()) -> {ok, atom(), tuple()}.
start_link(Statem_name) ->
    gen_statem:start_link({local,Statem_name},?MODULE,{up,computer_side,open},[]).


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
init(State) ->
    %% Set the initial state to be the list of available Worker_ids
    %% and types.
    {ok,State,{name,[]}}.
    % {ok,ready,Worker_ids}.
%% @private
callback_mode() -> handle_event_function.

%%% state callback(s)
-spec down(term()) -> ok.
down(Boxer) ->
    % gen_statem:call(gen_statem:call(distributer,get_next),down).
    gen_statem:call(Boxer,down).

up(Boxer) -> gen_statem:call(Boxer,up).

go_to_box_side(Boxer) -> gen_statem:call(Boxer,go_to_box_side).

go_to_computer_side(Boxer) -> gen_statem:call(Boxer,go_to_computer_side).

close(Boxer) -> gen_statem:call(Boxer,close).

open(Boxer) -> gen_statem:call(Boxer,open).
%%
%% Used to select which registered worker is to be used next in 
%% a round robin fashion.
%% @private
% handle_event({call,From}, next, ready,{Statem_name,State_data}) ->
%     %Modify the state data and replace State_data below with the modified state data.
%     {next_state, ready,{Statem_name,State_data},[{reply,From,Statem_name}]}.

% handle_event({call,From}, down, {up,computer_side,open}, _) ->
handle_event({call,From}, down, {up,computer_side,open}, {_Statem_name,State_data}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, {down,computer_side,open},{{down,computer_side,open},State_data},[{reply,From,is_down}]};
    % {reply,
    %     is_down, 
    %     {down,computer_side,open}};
handle_event({call,From}, down, {up,box_side,close}, {_Statem_name,State_data}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, {down,box_side,close},{{down,box_side,close},State_data},[{reply,From,is_down}]};
    % {reply, 
    %     is_down,
    %     {down,box_side,close}};

handle_event({call,From}, up, {down,computer_side,close},{_Statem_name,State_data}) ->
    %Modify the state data and replace State_data below with the modified state data.  
    {next_state, {up,computer_side,close},{{up,computer_side,close},State_data},[{reply,From,is_up}]};
    % {reply, 
    %     is_up,
    %     {up,computer_side,close}};

handle_event({call,From}, up, {down,box_side,open},{_Statem_name,State_data}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, {up,box_side,open},{{up,box_side,open},State_data},[{reply,From,is_up}]};
    % {reply, 
    %     is_up,
    %     {up,box_side,open}};

handle_event({call,From}, go_to_box_side, {up,computer_side,close},{_Statem_name,State_data}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, {up,box_side,close},{{up,box_side,close},State_data},[{reply,From,is_over_box}]};
    % {reply, 
    %     is_over_box,
    %     {up,box_side,close}};

handle_event({call,From}, go_to_computer_side, {up,box_side,open}, {_Statem_name,State_data}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, {up,computer_side,open},{{up,computer_side,open},State_data},[{reply,From,is_over_computer}]};
    % {reply, 
    %     is_over_computer,
    %     {up,computer_side,open}};

% handle_event({call,From}, close, {down,computer_side,open},_) ->
handle_event({call,From}, close, {down,computer_side,open},{_Statem_name,State_data}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, {down,computer_side,close},{{down,computer_side,close},State_data},[{reply,From,is_closed}]};
    % {reply,
    %     is_closed,
    %     {down,computer_side,close}};

handle_event({call,From}, open, {down,box_side,close},{_Statem_name,State_data}) ->
    %Modify the state data and replace State_data below with the modified state data.
    {next_state, {down,box_side,open},{{down,box_side,open},State_data},[{reply,From,is_open}]}.
    % {reply, 
    %     is_open,
    %     {down,box_side,open}}.

%% This code is included in the compiled code only if 
%% 'rebar3 eunit' is being executed.
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
%%
open_test() ->
    {setup,
        fun() -> gen_statem:start_link({local,floyd},?MODULE,{up,computer_side,open},[]) end,
        fun() -> gen_server:stop(floyd) end,
        [
            ?assertMatch({next_state, {down,box_side,open},{{down,box_side,open},_},[{reply,_,is_open}]}, handle_event({call,coach},open,{down,box_side,close},{floyd,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},open,{up,computer_side,open},{floyd,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},1,{down,box_side,close},{floyd,state_data}))
        ]}.

close_test() ->
    {setup,
        fun() -> gen_statem:start_link({local,floyd_close},?MODULE,{up,computer_side,open},[]) end,
        fun() -> gen_server:stop(floyd_close) end,
        [
            ?assertMatch({next_state, {down,computer_side,close},{{down,computer_side,close},_},[{reply,_,is_closed}]}, handle_event({call,coach},close,{down,computer_side,open},{floyd_close,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},close,{up,computer_side,open},{floyd_close,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},1,{down,computer_side,open},{floyd_close,state_data}))
        ]}.

down_test() ->
    {setup,
        fun() -> gen_statem:start_link({local,floyd_down},?MODULE,{up,computer_side,open},[]) end,
        fun() -> gen_server:stop(floyd_close) end,
        [
            ?assertMatch({next_state, {down,computer_side,open},{{down,computer_side,open},_},[{reply,_,is_down}]}, handle_event({call,coach},down,{up,computer_side,open},{floyd_down,state_data})),
            ?assertMatch({next_state, {down,box_side,close},{{down,box_side,close},_},[{reply,_,is_down}]}, handle_event({call,coach},down,{up,box_side,close},{floyd_down,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},down,{down,computer_side,open},{floyd_down,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},1,{down,computer_side,open},{floyd_down,state_data}))
        ]}.
        
go_to_box_side_test() ->
    {setup,
        fun() -> gen_statem:start_link({local,floyd_box_side},?MODULE,{up,computer_side,open},[]) end,
        fun() -> gen_server:stop(floyd_box_side) end,
        [
            ?assertMatch({next_state, {up,box_side,close},{{up,box_side,close},_},[{reply,_,is_over_box}]}, handle_event({call,coach},go_to_box_side,{up,computer_side,close},{floyd_box_side,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},go_to_box_side,{up,computer_side,open},{floyd_box_side,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},1,{down,computer_side,open},{floyd_box_side,state_data}))
        ]}.

go_to_computer_side_test() ->
    {setup,
        fun() -> gen_statem:start_link({local,floyd_computer_side},?MODULE,{up,computer_side,open},[]) end,
        fun() -> gen_server:stop(floyd_computer_side) end,
        [
            ?assertMatch({next_state, {up,computer_side,open},{{up,computer_side,open},_},[{reply,_,is_over_computer}]},handle_event({call,coach},go_to_computer_side,{up,box_side,open},{floyd_box_side,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},go_to_computer_side,{up,computer_side,open},{floyd_box_side,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},1,{up,box_side,open},{floyd_down,state_data}))
        ]}.

up_test() ->
    {setup,
        fun() -> gen_statem:start_link({local,floyd_up},?MODULE,{up,computer_side,open},[]) end,
        fun() -> gen_server:stop(floyd_close) end,
        [
            ?assertMatch({next_state, {up,computer_side,close},{{up,computer_side,close},_},[{reply,_,is_up}]}, handle_event({call,coach},up,{down,computer_side,close},{floyd_up,state_data})),
            ?assertMatch({next_state, {up,box_side,open},{{up,box_side,open},_},[{reply,_,is_up}]}, handle_event({call,coach},up,{down,box_side,open},{floyd_up,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},up,{down,computer_side,open},{floyd_up,state_data})),
            ?assertException(error, function_clause, handle_event({call,coach},1,{down,computer_side,open},{floyd_up,state_data}))
        ]}.

%%
-endif.
