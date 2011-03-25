-module(pteracuda_nifs).

-define(NIF_API_VERSION, 1).
-define(MISSING_NIF, throw({error, missing_nif})).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-on_load(init/0).

-export([init/0]).

%% API
-export([new_context/0,
         new_context/1,
         destroy_context/1]).

-export([new_int_buffer/0,
         new_string_buffer/0,
         new_float_buffer/0,
         destroy_buffer/1,
         buffer_size/1]).

-export([write_buffer/2,
         buffer_delete/2,
         buffer_insert/3,
         read_buffer/1,
         clear_buffer/1,
         copy_buffer/2]).

-export([sort_buffer/2,
         buffer_contains/3,
         buffer_intersection/3,
         buffer_minmax/2]).

new_context() ->
    ?MISSING_NIF.

new_context(_DeviceNum) ->
    ?MISSING_NIF.

destroy_context(_Ctx) ->
    ?MISSING_NIF.

new_int_buffer() ->
    ?MISSING_NIF.

new_string_buffer() ->
    ?MISSING_NIF.

new_float_buffer() ->
    ?MISSING_NIF.

destroy_buffer(_Buffer) ->
    ?MISSING_NIF.

buffer_size(_Buffer) ->
    ?MISSING_NIF.

read_buffer(_Buffer) ->
    ?MISSING_NIF.

write_buffer(_Buffer, _Data) ->
    ?MISSING_NIF.

buffer_delete(_Buffer, _Pos) ->
    ?MISSING_NIF.

buffer_insert(_Buffer, _Pos, _Value) ->
    ?MISSING_NIF.

sort_buffer(_Ctx, _Buffer) ->
    ?MISSING_NIF.

clear_buffer(_Buffer) ->
    ?MISSING_NIF.

copy_buffer(_From, _To) ->
    ?MISSING_NIF.

buffer_contains(_Ctx, _Buffer, _Value) ->
    ?MISSING_NIF.

buffer_intersection(_Ctx, _First, _Second) ->
    ?MISSING_NIF.

buffer_minmax(_Ctx, _Buffer) ->
    ?MISSING_NIF.

init() ->
    PrivDir = case code:priv_dir(pteracuda) of
                  {error, bad_name} ->
                      D = filename:dirname(code:which(?MODULE)),
                      filename:join([D, "..", "priv"]);
                  Dir ->
                      Dir
              end,
    SoName = filename:join([PrivDir, "pteracuda_nifs"]),
    erlang:load_nif(SoName, ?NIF_API_VERSION).

-ifdef(TEST).

create_destroy_test() ->
    {ok, Buf} = pteracuda_nifs:new_int_buffer(),
    ok = pteracuda_nifs:destroy_buffer(Buf).

create_destroy_float_test() ->
    {ok, Buf} = pteracuda_nifs:new_float_buffer(),
    ok = pteracuda_nifs:destroy_buffer(Buf).

create_write_destroy_test() ->
    {ok, Buf} = pteracuda_nifs:new_int_buffer(),
    pteracuda_nifs:write_buffer(Buf, [1,2,3,4,5]),
    {ok, 5} = pteracuda_nifs:buffer_size(Buf),
    ok = pteracuda_nifs:destroy_buffer(Buf).

create_write_destroy_float_test() ->
    {ok, Buf} = pteracuda_nifs:new_float_buffer(),
    pteracuda_nifs:write_buffer(Buf, [0.01, 0.002, 0.0003, 0.4, 1.5]),
    {ok, 5} = pteracuda_nifs:buffer_size(Buf),
    ok = pteracuda_nifs:destroy_buffer(Buf).

create_write_delete_test() ->
    {ok, Buf} = pteracuda_nifs:new_int_buffer(),
    ok = pteracuda_nifs:write_buffer(Buf, [1,2,3,4,5]),
    ok = pteracuda_nifs:buffer_delete(Buf, 1),
    {ok, [1,3,4,5]} = pteracuda_nifs:read_buffer(Buf),
    ok = pteracuda_nifs:buffer_delete(Buf, 0),
    {ok, [3,4,5]} = pteracuda_nifs:read_buffer(Buf),
    pteracuda_nifs:destroy_buffer(Buf).

create_write_delete_float_test() ->
    {ok, Buf} = pteracuda_nifs:new_float_buffer(),
    ok = pteracuda_nifs:write_buffer(Buf, [1.1,1.2,1.3,1.4,1.5]),
    ok = pteracuda_nifs:buffer_delete(Buf, 1),
    {ok, [1.1,1.3,1.4,1.5]} = pteracuda_nifs:read_buffer(Buf),
    ok = pteracuda_nifs:buffer_delete(Buf, 0),
    {ok, [1.3,1.4,1.5]} = pteracuda_nifs:read_buffer(Buf),
    pteracuda_nifs:destroy_buffer(Buf).

insert_test() ->
    {ok, Buf} = pteracuda_nifs:new_int_buffer(),
    ok = pteracuda_nifs:buffer_insert(Buf, 0, 1),
    error = pteracuda_nifs:buffer_insert(Buf, 5, 2),
    {ok, [1]} = pteracuda_nifs:read_buffer(Buf),
    ok = pteracuda_nifs:clear_buffer(Buf),
    ok = pteracuda_nifs:write_buffer(Buf, [1,2,3,4,5]),
    ok = pteracuda_nifs:buffer_insert(Buf, 2, 6),
    {ok, [1,2,6,3,4,5]} = pteracuda_nifs:read_buffer(Buf),
    pteracuda_nifs:destroy_buffer(Buf).

insert_float_test() ->
    {ok, Buf} = pteracuda_nifs:new_float_buffer(),
    ok = pteracuda_nifs:buffer_insert(Buf, 0, 1.0),
    error = pteracuda_nifs:buffer_insert(Buf, 5, 2.0),
    {ok, [1.0]} = pteracuda_nifs:read_buffer(Buf),
    ok = pteracuda_nifs:clear_buffer(Buf),
    ok = pteracuda_nifs:write_buffer(Buf, [1.0,2.0,3.0,4.0,5.0]),
    ok = pteracuda_nifs:buffer_insert(Buf, 2, 6.0),
    {ok, [1.0,2.0,6.0,3.0,4.0,5.0]} = pteracuda_nifs:read_buffer(Buf),
    pteracuda_nifs:destroy_buffer(Buf).

create_write_sort_destroy_test() ->
    {ok, Buf} = pteracuda_nifs:new_int_buffer(),
    {ok, Ctx} = pteracuda_nifs:new_context(),
    ok = pteracuda_nifs:write_buffer(Buf, [3,2,1,4,5]),
    {ok, 5} = pteracuda_nifs:buffer_size(Buf),
    ok = pteracuda_nifs:sort_buffer(Ctx, Buf),
    {ok, [1,2,3,4,5]} = pteracuda_nifs:read_buffer(Buf),
    ok = pteracuda_nifs:destroy_buffer(Buf),
    ok = pteracuda_nifs:destroy_context(Ctx).

create_write_sort_destroy_float_test() ->
    {ok, Buf} = pteracuda_nifs:new_float_buffer(),
    {ok, Ctx} = pteracuda_nifs:new_context(),
    ok = pteracuda_nifs:write_buffer(Buf, [3.1,2.1,1.1,4.1,5.1]),
    {ok, 5} = pteracuda_nifs:buffer_size(Buf),
    ok = pteracuda_nifs:sort_buffer(Ctx, Buf),
    {ok, [1.1,2.1,3.1,4.1,5.1]} = pteracuda_nifs:read_buffer(Buf),
    ok = pteracuda_nifs:destroy_buffer(Buf),
    ok = pteracuda_nifs:destroy_context(Ctx).

create_write_clear_test() ->
    {ok, Buf} = pteracuda_nifs:new_int_buffer(),
    ok = pteracuda_nifs:write_buffer(Buf, [3,2,1,4,5]),
    {ok, 5} = pteracuda_nifs:buffer_size(Buf),
    pteracuda_nifs:clear_buffer(Buf),
    {ok, 0} = pteracuda_nifs:buffer_size(Buf),
    ok = pteracuda_nifs:destroy_buffer(Buf).

create_write_contains_test() ->
    {ok, Buf} = pteracuda_nifs:new_int_buffer(),
    {ok, Ctx} = pteracuda_nifs:new_context(),
    N = lists:seq(1, 1000),
    ok = pteracuda_nifs:write_buffer(Buf, N),
    true = pteracuda_nifs:buffer_contains(Ctx, Buf, 513),
    false = pteracuda_nifs:buffer_contains(Ctx, Buf, 1500),
    ok = pteracuda_nifs:destroy_buffer(Buf),
    ok = pteracuda_nifs:destroy_context(Ctx).

create_write_contains_float_test() ->
    {ok, Buf} = pteracuda_nifs:new_float_buffer(),
    {ok, Ctx} = pteracuda_nifs:new_context(),
    N = [X + 0.0001 || X <- lists:seq(1, 1000)],
    ok = pteracuda_nifs:write_buffer(Buf, N),
    true = pteracuda_nifs:buffer_contains(Ctx, Buf, 513.0001),
    false = pteracuda_nifs:buffer_contains(Ctx, Buf, 1500.0),
    ok = pteracuda_nifs:destroy_buffer(Buf),
    ok = pteracuda_nifs:destroy_context(Ctx).

create_copy_test() ->
    {ok, Buf} = pteracuda_nifs:new_int_buffer(),
    ok = pteracuda_nifs:write_buffer(Buf, lists:seq(1, 1000)),
    {ok, Buf1} = pteracuda_nifs:new_int_buffer(),
    ok = pteracuda_nifs:copy_buffer(Buf, Buf1),
    {ok, 1000} = pteracuda_nifs:buffer_size(Buf1),
    ok = pteracuda_nifs:destroy_buffer(Buf),
    ok = pteracuda_nifs:destroy_buffer(Buf1).

intersection_test() ->
    {ok, B1} = pteracuda_nifs:new_int_buffer(),
    {ok, B2} = pteracuda_nifs:new_int_buffer(),
    {ok, Ctx} = pteracuda_nifs:new_context(),
    ok = pteracuda_nifs:write_buffer(B1, lists:seq(1, 100)),
    ok = pteracuda_nifs:write_buffer(B2, lists:seq(90, 190)),
    {ok, IB} = pteracuda_nifs:buffer_intersection(Ctx, B1, B2),
    11 = length(IB),
    pteracuda_nifs:destroy_context(Ctx),
    pteracuda_nifs:destroy_buffer(B1),
    pteracuda_nifs:destroy_buffer(B2).

minmax_test() ->
    {ok, B} = pteracuda_nifs:new_int_buffer(),
    {ok, Ctx} = pteracuda_nifs:new_context(),
    F = fun(_, _) -> random:uniform(100) > 49 end,
    N = lists:sort(F, lists:seq(1, 5000)),
    pteracuda_nifs:write_buffer(B, N),
    pteracuda_nifs:sort_buffer(Ctx, B),
    {ok, {1, 5000}} = pteracuda_nifs:buffer_minmax(Ctx, B),
    pteracuda_nifs:destroy_buffer(B),
    pteracuda_nifs:destroy_context(Ctx).

-endif.
