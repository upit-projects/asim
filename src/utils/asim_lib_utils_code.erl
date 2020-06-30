%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_code).
-author("madalin").

%% API
-export([strip/1]).
-export([strip_add_dot/1]).

-export([eval/1]).
-export([eval/2]).

-export([guard_eval/1]).
-export([guard_eval/2]).

-export([always_eval/2]).
-export([always_eval/3]).

%% @doc Strip spaces from the beginning and end of the string we need to evaluate
strip(String) when erlang:is_binary(String) -> strip(erlang:binary_to_list(String));
strip(String) when erlang:is_list(String) -> strip(String, []).
strip([$\n|T], Acum) -> strip(T, Acum);
strip([$\r|T], Acum) -> strip(T, Acum);
strip([$\t|T], Acum) -> strip(T, Acum);
strip([$\s|T], Acum) -> strip(T, Acum);
strip([H|T], Acum) ->
    NewAcum = lists:append(Acum, [H]),
    strip(T, NewAcum);
strip([], Acum) -> Acum.

%% @doc Add trailing dot to the end of the specified string or binary if necessary
strip_add_dot(String) ->

    StripString = strip(String),
    case lists:last(StripString) of
        $. -> StripString;
        _ -> lists:append(StripString, [$.])
    end.

%% @doc Evaluate a string as Erlang code
eval(String) -> eval(String, erl_eval:new_bindings()).
eval(String, Bindings) when erlang:is_binary(String) -> eval(erlang:binary_to_list(String), Bindings);
eval(String, Bindings) when erlang:is_list(String) ->

    case erl_scan:string(String) of
        {ok,Scanned,_} ->
            case erl_parse:parse_exprs(Scanned) of
                {ok, Parsed} ->
                    case erl_eval:exprs(Parsed, Bindings) of
                        {value, Value, _NewBindings} -> {ok, Value};
                        _ -> {error, exprs}
                    end;
                _ -> {error, parse_exprs}
            end;
        _ -> {error, scan}
    end.

%% @doc Evaluate a string as Erlang code.
%% If invalid raise exception.
guard_eval(String) ->
    {ok, Value} = eval(String),
    Value.

guard_eval(String, Bindings) ->
    {ok, Value} = eval(String, Bindings),
    Value.

%% @doc Evaluate a string as Erlang code.
%% If invalid returns default.
always_eval(String, Default) ->
    case eval(String) of
        {ok, Value} -> Value;
        _ -> Default
    end.

always_eval(String, Default, Bindings) ->
    case eval(String, Bindings) of
        {ok, Value} -> Value;
        _ -> Default
    end.