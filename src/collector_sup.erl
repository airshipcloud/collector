-module(collector_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

rest() ->
    Dispatch = cowboy_router:compile([{'_', [
        {'_',  collector, []}
    ]}]),
    {ok, Port} = application:get_env(collector, port),
    Config = [rest_listener, 100,
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]],
    {rest, {cowboy, start_http, Config}, permanent, 5000, supervisor, [dynamic]}.

init([]) ->
    {ok, { {one_for_one, 5, 10}, [rest()]} }.
