%% Copyright (C) 2011 by Alexander Pantyukhov <alwx.main@gmail.com>
%%                       Dmitry Groshev       <lambdadmitry@gmail.com>
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(ecirca).
-export([new/1,
         set/3,
         get/2,
         push/2,
         slice/3]).

-on_load(nif_init/0).

-define(APPNAME, ?MODULE).
-define(LIBNAME, ?MODULE).
-define(NULLVAL, 16#FFFFFFFFFFFFFFFF).

-type res() :: <<>>.

-spec new(pos_integer()) -> {ok, res()}.
new(_Size) -> not_loaded(?LINE).

-spec set(res(), pos_integer(), pos_integer()) -> {ok, res()} |
                                                  {error, not_found}.
set(_Res, _I, _Val) -> not_loaded(?LINE).

-spec get(res(), pos_integer()) -> {ok, pos_integer()} |
                                   {error, not_found}.
get(_Res, _I) -> not_loaded(?LINE).

-spec push(res(), pos_integer()) -> {ok, res()}.
push(_Res, _Val) -> not_loaded(?LINE).

-spec slice(res(), pos_integer(), pos_integer()) -> [pos_integer()] |
                                                    {error, slice_too_big}.
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


