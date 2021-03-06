-module(augeas_tests).
-include_lib("eunit/include/eunit.hrl").

new_test() ->
    {ok, R} = augeas:new("/", "", 0),
    ?assertEqual(ok, augeas:close(R)).

get_test() ->
    {ok, R} = augeas:new("/", "", 0),
    ?assertEqual(ok, augeas:set(R, "/test/value", "0")),
    ?assertEqual("0", augeas:get(R, "/test/value")),
    augeas:close(R).

setm_test() ->
    {ok, R} = augeas:new("/", "", 0),
    ?assertEqual(ok, augeas:set(R, "/test/value", "0")),
    ?assertEqual(1, augeas:setm(R, "/test/value", ".", "1")),
    ?assertEqual("1", augeas:get(R, "/test/value")),
    augeas:close(R).

rm_test() ->
    {ok, R} = augeas:new("/", "", 0),
    ?assertEqual(ok, augeas:set(R, "/test/value", "0")),
    ?assertNotEqual(0, augeas:rm(R, "/test")),
    augeas:close(R).

mv_test() ->
    {ok, R} = augeas:new("/", "", 0),
    ?assertEqual(ok, augeas:set(R, "/test/value/0", "0")),
    ?assertEqual(ok, augeas:mv(R, "/test/value/0", "/test/value/1")),
    ?assertEqual("0", augeas:get(R, "/test/value/1")),
    augeas:close(R).

cp_test() ->
    {ok, R} = augeas:new("/", "", 0),
    ?assertEqual(ok, augeas:set(R, "/test/value/0", "0")),
    ?assertEqual(ok, augeas:cp(R, "/test/value/0", "/test/value/1")),
    ?assertEqual("0", augeas:get(R, "/test/value/1")),
    augeas:close(R).

match_test() ->
    {ok, R} = augeas:new("/", "", 0),
    MList = augeas:match(R, "/files/etc/hosts/*"),
    ?assertNotEqual(0, length(MList)),
    augeas:close(R).

save_test() ->
    {ok, R} = augeas:new("/", "", 0),
    ?assertEqual(ok, augeas:save(R)),
    augeas:close(R).
