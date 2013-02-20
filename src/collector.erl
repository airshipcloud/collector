-module(collector).

-export([init/3]).
-export([rest_init/2]).
-export([malformed_request/2]).
-export([allowed_methods/2]).
-export([forbidden/2]).
-export([options/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([read_json/2]).

-record(state, {config, cloudstore_path, cloudstore_token}).

init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {ok, Json} = file:read_file(filename:join(code:priv_dir(collector), "config.json")),
    Config = jiffy:decode(Json),
    {ok, Req, #state{config = Config}}.

malformed_request(Req, State) ->
    {CloudstorePath, Req0} = cowboy_req:path(Req),
    case cowboy_req:qs_val(<<"token">>, Req0) of
        {undefined, Req1} ->
            {true, Req1, State};
        {Token, Req1} ->
            {false, Req1, State#state{cloudstore_path = CloudstorePath, cloudstore_token = Token}}
    end.

allowed_methods(Req, State) ->
    {[<<"PUT">>], Req, State}.

options(Req, State) ->
    {ok, Req, State}.

forbidden(Req, State) ->
    {false, Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, write_json}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, read_json}], Req, State}.

read_json(Req, #state{config = Config, cloudstore_path = CloudstorePath, cloudstore_token = CloudstoreToken} = State) ->
    {ok, Json, Req0} = cowboy_req:body(Req),
    Jterm = jiffy:decode(Json),
    case get_json_value([<<"service">>], Jterm) of
        undefined ->
            return400(Req0, State);
        Service ->
            case get_json_value([<<"services">>, Service], Config) of
                undefined ->
                    return400(Req0, State);
                ServiceConfig ->
                    ClientId = get_json_value([<<"client">>, <<"id">>], ServiceConfig),
                    ClientSecret = get_json_value([<<"client">>, <<"secret">>], ServiceConfig),
                    case get_json_value([<<"request">>], Jterm) of
                        undefined ->
                            return400(Req0, State);
                        Request ->
                            case {get_json_value([<<"access_token">>], Jterm), get_json_value([<<"service_token">>], Jterm)} of
                                {undefined, undefined} ->
                                    return400(Req0, State);
                                {AccessToken, undefined} ->
                                    case get_json_value([<<"provider">>], Jterm) of
                                        undefined ->
                                            return400(Req0, State);
                                        Provider ->
                                            {ok, ServiceToken} = get_service_token(Provider, ClientId, ClientSecret, AccessToken),
                                            {ok, Obj} = run_service_request(Request, ServiceToken),
                                            ok = cloudstore_store(CloudstorePath, CloudstoreToken, Obj),
                                            {true, Req0, State}
                                    end;
                                {undefined, _ServiceToken} ->
                                    return400(Req0, State);
                                {_, _} ->
                                    return400(Req0, State)
                            end
                    end
            end
    end.

cloudstore_store(Path, Token, Obj) ->
    {ok, Endpoint} = application:get_env(collector, cloudstore_endpoint),
    Url = binary_to_list(iolist_to_binary([
        Endpoint, Path,
        <<"?token=">>, Token
    ])),
    Res = lhttpc:request(
        Url,
        "PUT",
        [{<<"Content-Type">>, <<"application/json">>}],
        Obj,
        5000
    ),
    {ok, {{204, _}, _, _Json}} = Res,
    ok.

run_service_request(Request, Token) ->
    Url = binary_to_list(iolist_to_binary([
        <<"https://api.singly.com">>, Request,
        <<"?access_token=">>, Token
    ])),
    Res = lhttpc:request(
        Url,
        "GET",
        [],
        5000
    ),
    {ok, {{200, _}, _, Json}} = Res,
    {ok, Json}.

get_service_token(Service, ClientId, ClientSecret, AccessToken) ->
    Url = binary_to_list(iolist_to_binary([
        <<"https://api.singly.com/auth/">>, Service,
        <<"/apply?client_id=">>, ClientId,
        <<"&client_secret=">>, ClientSecret,
        <<"&token=">>, AccessToken
    ])),
    Res = lhttpc:request(
        Url,
        "GET",
        [],
        5000
    ),
    {ok, {{200, _}, _, Json}} = Res,
    {ok, get_json_value([<<"access_token">>], jiffy:decode(Json))}.

get_json_value(_, undefined) ->
    undefined;
get_json_value([], Obj) ->
    Obj;
get_json_value([Key | Rest], {Obj}) ->
    get_json_value(Rest, proplists:get_value(Key, Obj));
get_json_value([Index | _Rest], []) when is_integer(Index) ->
    undefined;
get_json_value([Index | Rest], Obj) when is_integer(Index), is_list(Obj) ->
    get_json_value(Rest, lists:nth(Index + 1, Obj));
get_json_value([Key | Rest], Obj) when is_list(Obj) ->
    get_json_value(Rest, proplists:get_all_values(Key, Obj)).

return400(Req, State) ->
    {ok, Req0} = cowboy_req:reply(400, Req),
    {halt, Req0, State}.
