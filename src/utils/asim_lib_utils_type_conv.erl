%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Implements various type conversion functions in use all over our simulation
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_type_conv).

-export([to_list/1]).
-export([to_list_binaries/1]).
-export([to_binary/1]).
-export([to_integer/1]).
-export([to_float/1]).
-export([to_atom/1]).

-export([ip_address_to_list/1]).
-export([ip_address_to_binary/1]).

%% @spec to_list(Any) -> list()
%% @doc Convert most Erlang types to list.
%% Accepted parameter types are: list, binary, integer, float and atom.
to_list(Any) when is_binary(Any) ->	binary_to_list(Any);
to_list(Any) when is_integer(Any) -> integer_to_list(Any);
to_list(Any) when is_float(Any) -> float_to_list(Any, [{decimals, 6}, compact]);
to_list(Any) when is_atom(Any) -> atom_to_list(Any);
to_list(Any) when is_list(Any) -> Any.

%% @spec to_list_binaries(Any) -> [binary()]
%% @doc Convert a list of various terms to a list of Erlang binaries.
%% Accepted parameter types are: list.
to_list_binaries(Any) when erlang:is_list(Any) -> to_list_binaries(Any, []).
to_list_binaries([H|T], Acum) -> to_list_binaries(T, [to_binary(H) | Acum]);
to_list_binaries([], Acum) -> lists:reverse(Acum).

%% @spec to_binary(Any) -> binary()
%% @doc Convert most Erlang types to binary.
%% Accepted parameter types are: list, binary, integer, float and atom.
to_binary(Any) when is_integer(Any) -> list_to_binary(integer_to_list(Any));
to_binary(Any) when is_float(Any) -> float_to_binary(Any, [{decimals, 6}, compact]);
to_binary(Any) when is_atom(Any) -> atom_to_binary(Any,utf8);
to_binary(Any) when is_list(Any) -> list_to_binary(Any);
to_binary(Any) when is_binary(Any) -> Any.

%% @spec to_integer(Any) -> integer()
%% @doc Convert most Erlang types to integer
%% Accepted parameter types are: list, binary, integer, float and atom.
%% Throw badarg if parameter contains a bad representation of an integer.
to_integer(Any) when is_atom(Any) -> to_integer(atom_to_list(Any));
to_integer(Any) when is_binary(Any) -> to_integer(binary_to_list(Any));
to_integer(Any) when is_list(Any) -> list_to_integer(Any);
to_integer(Any) when is_float(Any) -> trunc(Any);
to_integer(Any) when is_integer(Any) -> Any.

%% @spec to_float(Any) -> float()
%% @doc Convert most Erlang types to float
%% Accepted parameter types are: list, binary, integer, float and atom.
%% Throw badarg if parameter contains a bad representation of a float.
to_float(Any) when is_atom(Any) -> to_float(atom_to_list(Any));
to_float(Any) when is_binary(Any) -> to_float(binary_to_list(Any));
to_float(Any) when is_list(Any) -> list_to_float(Any);
to_float(Any) when is_integer(Any) -> float(Any);
to_float(Any) when is_float(Any) -> Any.

%% @spec to_atom(Any) -> atom
%% @doc Convert most Erlang types to atom.
%% latin1 encoding is used and no translation of bytes in the binary is done.
to_atom(Any) when is_binary(Any) -> binary_to_atom(Any, latin1);
to_atom(Any) when is_list(Any) -> binary_to_atom(iolist_to_binary(Any), latin1);
to_atom(Any) when is_float(Any) -> binary_to_atom(float_to_binary(Any), latin1);
to_atom(Any) when is_integer(Any) -> binary_to_atom(integer_to_binary(Any), latin1);
to_atom(Any) when is_atom(Any) -> Any.

%% @spec ip_address_to_list(Any) -> list()
%% @doc Convert ip addres to list.
%% Throw 'badarg' if parameter contains a bad representation of an ip addres
ip_address_to_list(Ip) when is_list(Ip) ->
    ParseResult = inet:parse_address(Ip),
    case ParseResult of
        {ok, IPAddress} -> ip_address_to_list(IPAddress);
        _ -> throw(badarg)
    end;
ip_address_to_list(Ip) when is_binary(Ip) ->
    ParseResult = inet:parse_address(binary_to_list(Ip)),
    case ParseResult of
        {ok, IPAddress} -> ip_address_to_list(IPAddress);
        _ -> throw(badarg)
    end;
ip_address_to_list(IpAddress) ->
    NtoaResult = inet:ntoa(IpAddress),
    case NtoaResult of
        {error, _} -> throw(badarg);
        _ -> NtoaResult
    end.

%% @spec ip_address_to_binary(Any) -> binary()
%% @doc Convert ip_addres() to binary.
%% Throw 'badarg' if parameter contains a bad representation of an ip addres
ip_address_to_binary(Ip) when is_list(Ip) ->
    ParseResult = inet:parse_address(Ip),
    case ParseResult of
        {ok, IPAddress} -> ip_address_to_binary(IPAddress);
        _ -> throw(badarg)
    end;
ip_address_to_binary(Ip) when is_binary(Ip) ->
    ParseResult = inet:parse_address(binary_to_list(Ip)),
    case ParseResult of
        {ok, IPAddress} -> ip_address_to_binary(IPAddress);
        _ -> throw(badarg)
    end;
ip_address_to_binary(IpAddress) ->
    NtoaResult = inet:ntoa(IpAddress),
    case NtoaResult of
        {error, _} -> throw(badarg);
        _ -> list_to_binary(NtoaResult)
    end.