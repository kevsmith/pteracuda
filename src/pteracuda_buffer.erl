-module(pteracuda_buffer).

-include("pteracuda_internals.hrl").

-export([new/1,
         destroy/1,
         size/1,
         write/2,
         read/1,
         duplicate/1,
         clear/1,
         sort/2,
         contains/3,
         intersection/3,
         minmax/2]).

new(integer) ->
    {ok, Buf} = pteracuda_nifs:new_int_buffer(),
    {ok, #pc_buffer{type=integer, ref=Buf}};
new(float) ->
    {ok, Buf} = pteracuda_nifs:new_float_buffer(),
    {ok, #pc_buffer{type=float, ref=Buf}};
new(string) ->
    {ok, Buf} = pteracuda_nifs:new_string_buffer(),
    {ok, #pc_buffer{type=string, ref=Buf}}.

destroy(#pc_buffer{ref=Ref}) ->
    pteracuda_nifs:destroy_buffer(Ref),
    ok.

size(#pc_buffer{ref=Ref}) ->
    pteracuda_nifs:buffer_size(Ref).

write(#pc_buffer{ref=Ref, type=Type}, Data) when Type =:= integer orelse
                                                 Type =:= string orelse
                                                 Type =:= float ->
    pteracuda_nifs:write_buffer(Ref, Data).

read(#pc_buffer{ref=Ref}) ->
    pteracuda_nifs:read_buffer(Ref).

duplicate(#pc_buffer{ref=Ref, type=Type}) when Type =:= integer orelse
                                               Type =:= string orelse
                                               Type =:= float ->
    {ok, OtherBuf} = new(Type),
    pteracuda_nifs:copy_buffer(Ref, OtherBuf#pc_buffer.ref),
    {ok, OtherBuf}.

clear(#pc_buffer{ref=Ref}) ->
    pteracuda_nifs:clear_buffer(Ref).

sort(#pc_context{ref=Ctx}, #pc_buffer{ref=Buf}) ->
    pteracuda_nifs:sort_buffer(Ctx, Buf).

contains(#pc_context{ref=Ctx}, #pc_buffer{ref=Buf}, Value) ->
    pteracuda_nifs:buffer_contains(Ctx, Buf, Value).

intersection(#pc_context{ref=Ctx}, #pc_buffer{ref=Buf1}, #pc_buffer{ref=Buf2}) ->
    pteracuda_nifs:buffer_intersection(Ctx, Buf1, Buf2).

minmax(#pc_context{ref=Ctx}, #pc_buffer{ref=Buf}) ->
    pteracuda_nifs:buffer_minmax(Ctx, Buf).
