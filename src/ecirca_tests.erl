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


-module(ecirca_tests).
-include_lib("eunit/include/eunit.hrl").

-define(LARGE_VALUE, math:pow(2, 50)).
-define(BIG_VALUE, math:pow(10, 6)).

new_test_() ->
    [?_assertEqual({ok, <<>>}, ecirca:new(3)),
     ?_assertError(badarg, ecirca:new(-1)),
     ?_assertError(badarg, ecirca:new(0)),
     ?_assertError(badarg, ecirca:new(?LARGE_VALUE))].

%% -------------------------------------------------------------------

push_test_() ->
    {setup,
     fun () -> {ok, C} = ecirca:new(5), C end,
     fun (_) -> ok end,
     fun (C) -> {with, C,
                 [fun push_subtest_badarg/1,
                  fun push_subtest_val/1]}
     end}.

push_subtest_badarg(C) ->
    ?assertError(badarg, ecirca:push(foobar, 1)),
    ?assertError(badarg, ecirca:push(C, -1)),
    ?assertError(badarg, ecirca:push(C, ?LARGE_VALUE)).

push_subtest_val(C) ->
    [?assertEqual({ok, C}, ecirca:push(C, X)) || X <- lists:seq(1, 10)].

%% -------------------------------------------------------------------

get_test_() ->
    {setup,
     fun () -> {ok, C} = ecirca:new(5), C end,
     fun (_) -> ok end,
     fun (C) -> {with, C,
                 [fun get_subtest_badarg/1,
                  fun get_subtest_nfound/1,
                  fun get_subtest_val1/1,
                  fun get_subtest_val2/1]}
     end}.

get_subtest_badarg(C) ->
    ?assertError(badarg, ecirca:get(foobar, 1)),
    ?assertError(badarg, ecirca:get(C, -1)),
    ?assertError(badarg, ecirca:get(C, 0)),
    ?assertError(badarg, ecirca:get(C, ?LARGE_VALUE)),
    ?assertError(badarg, ecirca:get(C, 6)).

get_subtest_nfound(C) ->
    ?assertEqual({error, not_found}, ecirca:get(C, 1)),
    ?assertEqual(ok, ecirca:push(C, 13)),
    ?assertEqual({ok, 13}, ecirca:get(C, 1)),
    [?assertEqual({error, not_found}, ecirca:get(C, X))
     || X <- lists:seq(2, 5)].

get_subtest_val1(C) ->
    [?assertEqual({ok, C}, ecirca:push(C, X)) || X <- lists:seq(1, 5)],
     [?assertEqual({ok, 5 - X + 1}, ecirca:get(C, X))
     || X <- lists:seq(1, 5)].

get_subtest_val2(C) ->
    [?assertEqual({ok, C}, ecirca:push(C, X)) || X <- lists:seq(1, 10)],
    [?assertEqual({ok, 10 - X + 1}, ecirca:get(C, X))
     || X <- lists:seq(1, 5)].

%% -------------------------------------------------------------------

set_test_() ->
    {setup,
     fun () -> {ok, C} = ecirca:new(5), C end,
     fun (_) -> ok end,
     fun (C) -> {with, C,
                 [fun set_subtest_badarg/1,
                  fun set_subtest_nfound/1,
                  fun get_subtest_val/1]}
     end}.

set_subtest_badarg(C) ->
    ?assertError(badarg, ecirca:set(foobar, 1)),
    ?assertError(badarg, ecirca:set(C, -1)),
    ?assertError(badarg, ecirca:set(C, 0)),
    ?assertError(badarg, ecirca:set(C, ?LARGE_VALUE)),
    ?assertError(badarg, ecirca:set(C, 6)).

set_subtest_nfound(C) ->
    ?assertEqual({error, not_found}, ecirca:set(C, 1)),
    ?assertEqual(ok, ecirca:push(C, 13)),
    ?assertEqual({ok, 13}, ecirca:set(C, 1)),
    [?assertEqual({error, not_found}, ecirca:get(C, X))
     || X <- lists:seq(2, 5)].

get_subtest_val(C) ->
    [?assertEqual({ok, C}, ecirca:push(C, X)) || X <- lists:seq(1, 5)],
     [?assertEqual({ok, 5 - X + 1}, ecirca:get(C, X))
     || X <- lists:seq(1, 5)].
