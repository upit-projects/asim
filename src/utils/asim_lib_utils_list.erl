%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Implements various functions to deal with lists
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_list).

-export([is_ascii_string/1]).
-export([is_unicode_string/1]).

-export([contains_duplicate_keys/1]).
-export([contains_duplicate_keys/2]).

-export([join_to_list_with_spacing/2]).
-export([join_to_binary_with_spacing/2]).

-export([insert_spacing_element/2]).

%% @doc Returns true if the specified list is a string and contains only ASCII characters
is_ascii_string([]) -> true;
is_ascii_string([H|_]) when erlang:is_integer(H) =:= false -> false;
is_ascii_string([H|_]) when H < 0; H > 255 -> false;
is_ascii_string([_|T]) -> is_ascii_string(T).

%% @doc Returns true if the specified list is a string and contains only valid UNICODE characters
%% Returns error whenever:
%% - an integer greater than 16#10FFFF (the maximum Unicode character) is found
%% - an integer greater in the range 16#D800 to 16#DFFF (invalid range reserved for UTF-16 surrogate pairs) is found.
is_unicode_string([]) -> true;
is_unicode_string([H|_]) when erlang:is_integer(H) =:= false -> false;
is_unicode_string([H|_]) when H < 0; H > 16#10ffff -> false;
is_unicode_string([H|_]) when H >= 16#d800, H =< 16#dfff -> false;
is_unicode_string([_|T]) -> is_unicode_string(T).

%% @doc Returns true if the specified tuples list contains duplicate elements of the first element of the touples or false otherwise.
contains_duplicate_keys(List) -> contains_duplicate_keys(List,1).

%% @doc Returns true if the specified tuples list contains duplicate elements of the specified Nth element of the touples or false otherwise
contains_duplicate_keys(List,NthElement) ->
    OriginalLength = erlang:length(List),
    SortedList     = lists:ukeysort(NthElement, List),
    SortedLength   = erlang:length(SortedList),
    case SortedLength of
        OriginalLength -> false;
        _ -> true
    end.

%% @spec join_to_list_with_spacing(List,Element) -> list()
join_to_list_with_spacing(Items, Sep) when is_list(Sep) ->
    JoinedList  = join_to_list_with_spacing(Items, Sep, []),
    ReversedList= lists:reverse(JoinedList),
    lists:flatten(ReversedList);
join_to_list_with_spacing(Items, Sep) ->
    join_to_list_with_spacing(Items, asim_lib_utils_type_conv:to_list(Sep)).

join_to_list_with_spacing([], _Sep, Acc) ->
    Acc;
join_to_list_with_spacing([Head | []], _Sep, Acc) when is_list(Head) ->
    [Head | Acc];
join_to_list_with_spacing([Head | []], _Sep, Acc) ->
    BinHead = asim_lib_utils_type_conv:to_list(Head),
    [BinHead | Acc];
join_to_list_with_spacing([Head | Tail], Sep, Acc) when is_list(Head) ->
    join_to_list_with_spacing(Tail, Sep, [Sep, Head | Acc]);
join_to_list_with_spacing([Head | Tail], Sep, Acc) ->
    BinHead = asim_lib_utils_type_conv:to_list(Head),
    join_to_list_with_spacing(Tail, Sep, [Sep, BinHead | Acc]).

%% @spec join_to_binary_with_spacing(List,Element) -> binary()
join_to_binary_with_spacing(Items, Sep) when is_binary(Sep) ->
    JoinedList  = join_to_binary_with_spacing(Items, Sep, []),
    ReversedList= lists:reverse(JoinedList),
    erlang:iolist_to_binary(ReversedList);
join_to_binary_with_spacing(Items, Sep) ->
    join_to_binary_with_spacing(Items, asim_lib_utils_type_conv:to_binary(Sep)).

join_to_binary_with_spacing([], _Sep, Acc) ->
    Acc;
join_to_binary_with_spacing([Head | []], _Sep, Acc) when is_binary(Head) ->
    [Head | Acc];
join_to_binary_with_spacing([Head | []], _Sep, Acc) ->
    BinHead = asim_lib_utils_type_conv:to_binary(Head),
    [BinHead | Acc];
join_to_binary_with_spacing([Head | Tail], Sep, Acc) when is_binary(Head) ->
    join_to_binary_with_spacing(Tail, Sep, [Sep, Head | Acc]);
join_to_binary_with_spacing([Head | Tail], Sep, Acc) ->
    BinHead = asim_lib_utils_type_conv:to_binary(Head),
    join_to_binary_with_spacing(Tail, Sep, [Sep, BinHead | Acc]).

%% @spec insert_spacing_element(List,Element) -> list
%% @doc Insert a spacing element into the specified list.
%% Examples when spacing element is " & ":
%% [{123,45},23,[{apples}]] is converted to [{123678,45}, " & ", 23, " & ", [{apples}]]
%% "123456789" is converted to [49," & ",50," & ",51," & ",52," & ",53," & ",54," & ",55," & ",56," & ",57]
%% If you want to insert substrings in ascii strings remember to list:flatten(Result).
insert_spacing_element(List,Element) -> insert_spacing_element(List,Element,[]).
insert_spacing_element([],_Element,Result) -> Result;
insert_spacing_element([Last],_Element,Result) -> lists:append(Result,[Last]);
insert_spacing_element([H|T],Element,Result) ->
	NewResult = lists:append(lists:append(Result,[H]),[Element]),
	insert_spacing_element(T,Element,NewResult).

