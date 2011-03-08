-module(pteracuda_nifs).

-define(NIF_API_VERSION, 1).
-define(MISSING_NIF, throw({error, missing_nif})).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-on_load(init/0).

-export([init/0]).

%% API
-export([new_worker/0,
         destroy_worker/1,
         new_buffer/1,
         destroy_buffer/1,
         buffer_length/1]).

%% Data transfer API
-export([write_integers/2,
         read_integers/1,
         sort_integers/1]).

new_worker() ->
    ?MISSING_NIF.

destroy_worker(_Worker) ->
    ?MISSING_NIF.

new_buffer(_Worker) ->
    ?MISSING_NIF.

destroy_buffer(_Worker) ->
    ?MISSING_NIF.

write_integers(_Worker, _Nums) ->
    ?MISSING_NIF.

read_integers(_Worker) ->
    ?MISSING_NIF.

sort_integers(_Worker) ->
    ?MISSING_NIF.

buffer_length(_Worker) ->
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
    {ok, W} = pteracuda_nifs:new_worker(),
    ok = pteracuda_nifs:destroy_worker(W),
    %% Can destroy a worker just once
    error = pteracuda_nifs:destroy_worker(W).

int_alloc_destroy_test() ->
    {ok, W} = pteracuda_nifs:new_worker(),
    ok = pteracuda_nifs:new_buffer(W),
    ok = pteracuda_nifs:destroy_buffer(W),
    %% Can destroy a buffer just once
    error = pteracuda_nifs:destroy_buffer(W),
    ok.

int_alloc_write_destroy_test() ->
    {ok, W} = pteracuda_nifs:new_worker(),
    ok = pteracuda_nifs:new_buffer(W),
    ok = pteracuda_nifs:write_integers(W, [1,2,3,4,5]),
    ?assertMatch({ok, 5}, pteracuda_nifs:buffer_length(W)),
    ok = pteracuda_nifs:destroy_buffer(W).

int_alloc_write_read_destroy_test() ->
    {ok, W} = pteracuda_nifs:new_worker(),
    ok = pteracuda_nifs:new_buffer(W),
    ok = pteracuda_nifs:write_integers(W, [1,2,3,4,5]),
    {ok, [1,2,3,4,5]} = pteracuda_nifs:read_integers(W),
    ok = pteracuda_nifs:destroy_buffer(W).

-endif.
