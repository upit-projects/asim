%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%% @doc Contains functions for handling IP addressees.
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_utils_inet).
-author("madalin").

%% API
-export([list_to_ip_address/1]).
-export([binary_to_ip_address/1]).

-export([host_to_binary/1]).

-export([guard_list_to_ip_address/1]).
-export([guard_binary_to_ip_address/1]).

host_to_binary({A, B, C, D}) ->

    erlang:iolist_to_binary([erlang:integer_to_binary(A), <<".">>,
                             erlang:integer_to_binary(B), <<".">>,
                             erlang:integer_to_binary(C), <<".">>,
                             erlang:integer_to_binary(D)]);

host_to_binary({A, B, C, D, E, F, G, H, I, J}) ->

    erlang:iolist_to_binary([erlang:integer_to_binary(A), <<".">>,
                             erlang:integer_to_binary(B), <<".">>,
                             erlang:integer_to_binary(C), <<".">>,
                             erlang:integer_to_binary(D), <<".">>,
                             erlang:integer_to_binary(E), <<".">>,
                             erlang:integer_to_binary(F), <<".">>,
                             erlang:integer_to_binary(G), <<".">>,
                             erlang:integer_to_binary(H), <<".">>,
                             erlang:integer_to_binary(I), <<".">>,
                             erlang:integer_to_binary(J)]);

host_to_binary(Host) when erlang:is_atom(Host) -> erlang:atom_to_binary(Host, utf8);
host_to_binary(Host) when erlang:is_list(Host) -> erlang:list_to_binary(Host).

%% @doc Parse list containing ipv4 address or ipv6 address
%% Return {ok, Address} | {error, Reason}
list_to_ip_address(Host) when erlang:is_list(Host) -> inet_parse:address(Host).

%% @doc Parse binary containing ipv4 address or ipv6 address
%% Return {ok, Address} | {error, Reason}
binary_to_ip_address(Host) when erlang:is_binary(Host) -> inet_parse:address(erlang:binary_to_list(Host)).

%% @doc Parse list containing ipv4 address or ipv6 address
%% Return {ok, Address} | {error, Reason}
guard_list_to_ip_address(Host) when erlang:is_list(Host) ->

    {ok, Ip} = list_to_ip_address(Host),
    Ip.

%% @doc Parse binary containing ipv4 address or ipv6 address
%% Return {ok, Address} | {error, Reason}
guard_binary_to_ip_address(Host) when erlang:is_binary(Host) ->

    {ok, Ip} = binary_to_ip_address(Host),
    Ip.


