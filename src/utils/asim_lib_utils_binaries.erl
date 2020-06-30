%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_utils_binaries).
-author("madalin").

%% API
-export([is_binary_not_empty/1]).
-export([reverse/1]).
-export([add_padding/3]).
-export([explode_by_byte/2]).

-export([ltrim_byte/2]).

-export([resize_cutend_or_prepend/3]).

-export([part_from_end/2]).
-export([part_from_begining/2]).
-export([prepend_byte_to_size/3]).

%% @doc Returns true if Term is a non empty binary, otherwise false.
is_binary_not_empty(<<>>) -> false;
is_binary_not_empty(Term) -> erlang:is_binary(Term).

%% @doc Reverse the specified binary
reverse(Binary) -> reverse(<<>>, Binary).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) -> reverse(Rest, <<H/binary, Acc/binary>>).

%% @doc Add padding to the specified binary
add_padding(Binary, Length, Byte) when is_binary(Binary), is_integer(Length), is_integer(Byte) ->
    private_add_padding(Binary, Length, Byte).
private_add_padding(Binary, Length, Byte) when Length > 0 ->
    private_add_padding(<<Binary/binary, Byte:8>>, Length-1, Byte);
private_add_padding(Binary, _Length, _Byte) -> Binary.

%% @doc Extracts Len bytes of Binary from end
%% If the binary size is less original binary is returned untouched.
part_from_end(Binary, Len) when
    is_binary(Binary),
    is_integer(Len) ->
    OriginalSize = erlang:size(Binary),
    Pos          = OriginalSize - Len,
    case Pos > 0 of
        true ->
            binary:part(Binary, Pos, Len);
        _ -> Binary
    end.

%% @doc Extracts Len bytes of Binary from end
%% If the binary size is less original binary is returned untouched.
part_from_begining(Binary, Length) when
    is_binary(Binary),
    is_integer(Length) ->
    OriginalSize = erlang:size(Binary),
    case Length < OriginalSize of
        true -> binary:part(Binary, 0, Length);
        _ -> Binary
    end.

%% @doc Prepend the specified byte to the specified binary in order to make sure the binary has the specified minimum size
%% If the binary size is bigger original binary is returned untouched.
prepend_byte_to_size(Binary, Size, Byte) when
    is_binary(Binary),
    is_integer(Byte),
    is_integer(Size) ->
    OriginalSize = erlang:size(Binary),
    HowMany      = Size - OriginalSize,
    case HowMany > 0 of
        true ->
            Chunk = repeat_byte(HowMany, Byte),
            <<Chunk/binary, Binary/binary>>;
        _ -> Binary
    end.

%% @doc Resize the specified binary to the specified size
%% If the binary size is bigger then the specified size the exciding bytes are removed from the binary end.
%% If the binary size is less then the specified size the specify byte is added to the beginning of the binary.
%% If current binary size match the requested size original binary is returned untoched.
resize_cutend_or_prepend(Binary, NewSize, Byte) when
    is_binary(Binary),
    is_integer(NewSize),
    is_integer(Byte) ->
    OriginalSize = erlang:size(Binary),
    case NewSize of
        OriginalSize -> Binary;
        _ ->
            Diff = NewSize - OriginalSize,
            case Diff > 0 of
                true ->

                    %% Add bytes
                    Chunk = repeat_byte(Diff, Byte),
                    <<Chunk/binary, Binary/binary>>;
                _ ->

                    %% Cut
                    binary:part(Binary, 0, NewSize)

        end
    end.

%% @doc Creates binary by repeating the specified byte
repeat_byte(HowMany, Byte) -> repeat_byte(HowMany, Byte, <<>>).
repeat_byte(HowMany, Byte, Acum) when HowMany > 0 ->
    repeat_byte(HowMany-1, Byte, <<Byte:8, Acum/binary>>);
repeat_byte(_HowMany, _Byte, Acum) -> Acum.

%% @doc Explode binary by specified byte
%% Returns a list of exploded binaries.
explode_by_byte(Binary, Byte) when erlang:is_binary(Binary), erlang:is_integer(Byte) -> explode_by_byte(Binary, Byte, <<>>, []);
explode_by_byte(Binary, <<Byte>>) when erlang:is_binary(Binary) -> explode_by_byte(Binary, Byte, <<>>, []).
explode_by_byte(<<>>, _Byte, <<>>, AcumL) -> AcumL;
explode_by_byte(<<>>, _Byte, AcumB, []) -> AcumB;
explode_by_byte(<<>>, _Byte, AcumB, AcumL) -> lists:append(AcumL, [AcumB]);
explode_by_byte(<<NextByte,Rest/binary>>, Byte, AcumB, AcumL) ->
    case NextByte of
        Byte ->
            NewAcumL = lists:append(AcumL, [AcumB]),
            explode_by_byte(Rest, Byte, <<>>, NewAcumL);
        _ ->
            NewAcumB = <<AcumB/binary, NextByte>>,
            explode_by_byte(Rest, Byte, NewAcumB, AcumL)
    end.

%% @doc Left trim the specified byte from the specified binary
ltrim_byte(Byte, <<Byte, Rest/binary>>) -> ltrim_byte(Byte, Rest);
ltrim_byte(_Byte, Rest) -> Rest.