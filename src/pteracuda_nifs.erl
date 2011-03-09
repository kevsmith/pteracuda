-module(pteracuda_nifs).

-define(NIF_API_VERSION, 1).
-define(MISSING_NIF, throw({error, missing_nif})).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-on_load(init/0).

-export([init/0]).

%% API
-export([new_buffer/0,
         destroy_buffer/1,
         buffer_size/1]).

-export([write_buffer/2,
         read_buffer/1]).

-export([sort_buffer/1]).

new_buffer() ->
    ?MISSING_NIF.

destroy_buffer(_Buffer) ->
    ?MISSING_NIF.

buffer_size(_Buffer) ->
    ?MISSING_NIF.

read_buffer(_Buffer) ->
    ?MISSING_NIF.

write_buffer(_Buffer, _Data) ->
    ?MISSING_NIF.

sort_buffer(_Buffer) ->
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
    {ok, Buf} = pteracuda_nifs:new_buffer(),
    ok = pteracuda_nifs:destroy_buffer(Buf).

create_write_destroy_test() ->
    {ok, Buf} = pteracuda_nifs:new_buffer(),
    pteracuda_nifs:write_buffer(Buf, [1,2,3,4,5]),
    {ok, 5} = pteracuda_nifs:buffer_size(Buf),
    ok = pteracuda_nifs:destroy_buffer(Buf).

create_write_sort_destroy_test() ->
    {ok, Buf} = pteracuda_nifs:new_buffer(),
    ok = pteracuda_nifs:write_buffer(Buf, [3,2,1,4,5]),
    {ok, 5} = pteracuda_nifs:buffer_size(Buf),
    ok = pteracuda_nifs:sort_buffer(Buf),
    {ok, [1,2,3,4,5]} = pteracuda_nifs:read_buffer(Buf),
    ok = pteracuda_nifs:destroy_buffer(Buf).

-endif.
