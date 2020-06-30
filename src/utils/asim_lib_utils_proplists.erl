%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Contains helper functions for handling proplists.
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_proplists).
-author("madalin").

%% API
-export([guard_get_value/2]).
-export([guard_get_value/3]).

-export([guard_get_sublist/1]).
-export([guard_get_sublist/2]).

-export([guard_get_binary/2]).
-export([guard_get_binary/3]).

-export([guard_get_integer/2]).
-export([guard_get_integer/3]).

-export([guard_get_list/2]).
-export([guard_get_list/3]).

-export([guard_get_atom/2]).
-export([guard_get_atom/3]).

-export([delete_all_existing_and_append/2]).
-export([delete_all_existing_and_append/3]).

-export([delete_all_existing_and_prepend/2]).

-export([convert_key_to_atom/1]).
-export([convert_key_to_binary/1]).


%% @doc Get sublist from list when the list is coming from an term converted from json
guard_get_sublist([{List}]) -> List;
guard_get_sublist(_) -> throw(proplists_error_missing_key).

%% @doc Get sublist from list when the list is comming from an term converted from json
%% The specified exception is thrown if can not get sublist.
guard_get_sublist([{List}], _Exception) -> List;
guard_get_sublist(_, Exception) -> throw(Exception).

%% @doc Get value from proplist
guard_get_value(Key, List) when erlang:is_list(List) ->
    Result = proplists:get_value(Key, List),
    case Result of
        undefined -> throw({proplists_error_missing_key, Key});
        _ -> Result
    end.

%% @doc Get value from proplist throwing the specified exception if value is not found
guard_get_value(Key, List, Exception) when erlang:is_list(List) ->
    Result = proplists:get_value(Key, List),
    case Result of
        undefined -> throw(Exception);
        _ -> Result
    end.

%% @doc Get integer value from proplist throwing the specified exception if value is not found
guard_get_integer(Key, List) -> guard_get_integer(Key, List, proplists_error_invalid).
guard_get_integer(Key, List, Exception) when erlang:is_list(List) ->
    Result = proplists:get_value(Key, List),
    case Result of
        undefined -> throw(Exception);
        _ ->
            case erlang:is_integer(Result) of
                true -> Result;
                false -> throw(Exception)
            end
    end.

%% @doc Get list value from proplist throwing the specified exception if value is not found
guard_get_list(Key, List) -> guard_get_list(Key, List, proplists_error_invalid).
guard_get_list(Key, List, Exception) when erlang:is_list(List) ->
    Result = proplists:get_value(Key, List),
    case Result of
        undefined -> throw(Exception);
        _ ->
            case erlang:is_list(Result) of
                true -> Result;
                false -> throw(Exception)
            end
    end.

%% @doc Get value from proplist throwing the specified exception if value is not found
guard_get_binary(Key, List) -> guard_get_binary(Key, List, proplists_error_invalid).
guard_get_binary(Key, List, Exception) when erlang:is_list(List) ->
    Result = proplists:get_value(Key, List),
    case Result of
        undefined -> throw(Exception);
        _ -> asim_lib_utils_type_conv:to_binary(Result)
    end.

%% @doc Get value from proplist throwing the specified exception if value is not found
guard_get_atom(Key, List) -> guard_get_atom(Key, List, proplists_error_invalid).
guard_get_atom(Key, List, Exception) when erlang:is_list(List) ->
    Result = proplists:get_value(Key, List),
    case Result of
        undefined -> throw(Exception);
        _ -> asim_lib_utils_type_conv:to_atom(Result)
    end.

%% @doc Replace the specified key or append a new value at the end of the proplist
delete_all_existing_and_append([], List) when erlang:is_list(List) -> List;
delete_all_existing_and_append([{Key, Value}|T], List) when erlang:is_list(List) ->
    NewList = delete_all_existing_and_append(Key, Value, List),
    delete_all_existing_and_append(T, NewList).

delete_all_existing_and_append(Key, Value, List) when erlang:is_list(List) ->

    case proplists:is_defined(Key, List) of
        true ->
            NewList = proplists:delete(Key, List),
            lists:append(NewList, [{Key, Value}]);
        false -> lists:append(List, [{Key, Value}])
    end.

%% @doc Delete all existing keys and preped the new ones at the begining of the proplist
delete_all_existing_and_prepend([], List) when erlang:is_list(List) -> List;
delete_all_existing_and_prepend([{Key, Value}|T], List) when erlang:is_list(List) ->
    NewList = delete_all_existing_and_prepend(Key, Value, List),
    delete_all_existing_and_prepend(T, NewList);
delete_all_existing_and_prepend([_|T], List) when erlang:is_list(List) ->
    delete_all_existing_and_prepend(T, List).

delete_all_existing_and_prepend(Key, Value, List) when erlang:is_list(List) ->

    case proplists:is_defined(Key, List) of
        true ->
            NewList = proplists:delete(Key, List),
            [{Key, Value} | NewList];
        false -> [{Key, Value} | List]
    end.

%% @doc Convert proplists key to atom
convert_key_to_atom(List) -> convert_key_to_atom(List, []).
convert_key_to_atom([], Acum) -> Acum;
convert_key_to_atom([{Key, Value}|T], Acum) ->

    AtomKey     = asim_lib_utils_type_conv:to_atom(Key),
    NewAcum     = lists:append(Acum, [{AtomKey, Value}]),
    convert_key_to_atom(T, NewAcum).

%% @doc Convert proplists key to binary
convert_key_to_binary(List) -> convert_key_to_binary(List, []).
convert_key_to_binary([], Acum) -> Acum;
convert_key_to_binary([{Key, Value}|T], Acum) ->

    BinaryKey = asim_lib_utils_type_conv:to_binary(Key),
    NewAcum   = lists:append(Acum, [{BinaryKey, Value}]),
    convert_key_to_binary(T, NewAcum).