-module(ecirca_tests).
-include_lib("eunit/include/eunit.hrl").

-define(LARGE_VALUE, math:pow(2, 50)).

new_test_() ->
    [?_assertEqual(<<>>, ecirca:new(3)),
     ?_assertError(badarg, ecirca:new(-1)),
     ?_assertError(badarg, ecirca:new(0)),
     ?_assertError(badarg, ecirca:new(?LARGE_VALUE))].

get_test_() ->
    {setup,
     fun () -> ecirca:new(5) end,
     fun (_) -> ok end,
     fun (C) -> {with, C,
                 [fun get_subtest_badarg/1]}
     end}.

get_subtest_badarg(C) ->
    ?assertError(badarg, ecirca:get(foobar, 1)),
    ?assertError(badarg, ecirca:get(C, -1)),
    ?assertError(badarg, ecirca:get(C, 0)),
    ?assertError(badarg, ecirca:get(C, ?LARGE_VALUE)).


