-module(ecirca).
-export([new/1,
         set/3,
         get/2,
         push/2,
         slice/3]).
-export([start/0]).

-on_load(nif_init/0).

-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).
-define(NULLVAL, 16#FFFFFFFFFFFFFFFF).

-type res() :: any().

-spec new(pos_integer()) -> res().
new(_Size) -> not_loaded(?LINE).

-spec set(res(), pos_integer(), pos_integer()) -> {ok, res()}.
set(_Res, _I, _Val) -> not_loaded(?LINE).

-spec get(res(), pos_integer()) -> {ok, pos_integer()} |
                                   {error, not_found}.
get(_Res, _I) -> not_loaded(?LINE).

-spec push(res(), pos_integer()) -> {ok, res()}.
push(_Res, _Val) -> not_loaded(?LINE).

-spec slice(res(), pos_integer(), pos_integer()) -> [pos_integer()].
slice(_Res, _Start, _End) -> not_loaded(?LINE).

%% @doc Loads a NIF
-spec nif_init() -> ok | {error, _}.
nif_init() ->
    SoName = case code:priv_dir(?APPNAME) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                _ ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

start() ->
    T = circa:new(5),
    [circa:push(T, X) || X <- [1, 2, 3, 4]],
    [io:format("~snd elem: ~p", [X, circa:get(T, X)]) || X <- [1, 2, 3, 4, 5]].

