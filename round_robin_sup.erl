%%%-------------------------------------------------------------------
%% @doc round_robin top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(round_robin_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => top_dynamic_sup,
          start => {top_dynamic_sup,start,[local,top_dynamic_sup,[]]},% This template forces local registration of the child and
                                                  % forces it to startup without parameters. 
          restart => transient,
          shutdown => 1,
          type => worker,
          modules => [top_dynamic_sup]}
        %   #{id => robot,
        %     start => {robot,start_link,[robot,{up,computer_side,open}]},% This template forces local registration of the child and
        %                                           % forces it to startup without parameters. 
        %     restart => transient,
        %     shutdown => 1,
        %     type => worker,
        %     modules => [robot]}
        ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
