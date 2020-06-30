%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Implements various useful functions to deal with a HTTP request.
%%% Isolate us from the HTTP server we are using.
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_http_request).

-export([parse_qs/1]).
-export([parse_post/1]).
-export([parse_post/3]).
-export([parse_headers/1]).
-export([parse_cookies/1]).

-export([method/1]).
-export([peer/1]).
-export([peer_binary_address/1]).
-export([peer_binary_address_and_port/1]).
-export([path/1]).

-export([get_list/2]).
-export([get_ascii_list/2]).
-export([get_unicode_list/2]).
-export([get_binary/2]).
-export([get_integer/2]).
-export([get_float/2]).
%%-export([get_decoded_json/2]).

-export([always_get_list/3]).
-export([always_get_ascii_list/3]).
-export([always_get_unicode_list/3]).
-export([always_get_binary/3]).
-export([always_get_trimed_non_empty_binary/3]).
-export([always_get_trimed_utf8_binary/3]).
-export([always_get_integer/3]).
-export([always_get_integer_boolean/3]).
-export([always_get_float/3]).
%%-export([always_get_decoded_json/3]).

%% @spec parse_qs(Req) -> {Query, NewReq}
%% @doc Return the query string parsed as a list of tuples.
%%
%% This function will parse the query string.
parse_qs(Req) -> {cowboy_req:parse_qs(Req), Req}.

%% @spec parse_post(Req) -> {Post, NewReq}
%% @doc Return the request body as a list of tuples.
%%
%% This function will parse the body assuming the content-type application/x-www-form-urlencoded, commonly used for the query string.
%% By default it will attempt to read a body in one chunk, with a timeout of 60s.
parse_post(Req) -> parse_post(Req, 8000000, 60000).
parse_post(Req, ReadLength, Timeout) ->

	%% Not all requests come with a body
	case cowboy_req:has_body(Req) of
		true ->

			%% Read body
			case cowboy_req:read_urlencoded_body(Req, #{length => ReadLength, period => Timeout}) of
				{ok, Data, Req2} -> {Data, Req2};
				_ -> throw({http_error_bad_request, <<"Error parsing post">>})
			end;

		_ -> {[], Req}
	end.

%% @spec parse_headers(Req) -> {AllHeaders, NewReq}
%% @doc Return the HTTP headers.
parse_headers(Req) -> {maps:to_list(cowboy_req:headers(Req)), Req}.

%% @spec parse_cookies(Req) -> {AllCookies, NewReq}
%% @doc Return the HTTP headers.
parse_cookies(Req) -> {cowboy_req:parse_cookies(Req), Req}.

%% @spec method(Req) -> {Method, NewReq}
%% @doc Returns the method used for the specified request. Standard methods include GET, HEAD, OPTIONS, PATCH, POST, PUT, DELETE. Method names are case sensitive.
method(Req) -> {cowboy_req:method(Req), Req}.

%% @spec remote_ip(Req) -> {{IP, Port}, NewReq}
%% @doc Remote address and port number. This is not necessarily the actual IP and port of the client, but rather the one of the machine that connected to the server.
peer(Req) -> {cowboy_req:peer(Req), Req}.

%% @doc Remote address and port number. This is not necessarily the actual IP and port of the client, but rather the one of the machine that connected to the server.
peer_binary_address_and_port(Req) ->

    case cowboy_req:peer(Req) of
        {Adress, Port} ->
            NtoaAdress           = inet:ntoa(Adress),
            case NtoaAdress of
                {error, einval} -> throw(invalid_peer_adress);
                _ -> {{asim_lib_utils_type_conv:to_binary(NtoaAdress), asim_lib_utils_type_conv:to_binary(Port)}, Req}
            end;
        _ -> throw(invalid_peer_adress)
    end.

%% @doc Remote address.
%% This is not necessarily the actual IP and port of the client, but rather the one of the machine that connected to the server.
peer_binary_address(Req) ->

    case cowboy_req:peer(Req) of
        {Adress, _Port} ->
            NtoaAdress           = inet:ntoa(Adress),
            case NtoaAdress of
                {error, einval} -> throw(invalid_peer_adress);
                _ -> {asim_lib_utils_type_conv:to_binary(NtoaAdress), Req}
            end;
        _ -> throw(invalid_peer_adress)
    end.

path(Req) -> {cowboy_req:path(Req), Req}.

%%%%%%%%%
%% GET %%
%%%%%%%%%

%% @doc Returns value from request
%% Failure: throw(http_error_bad_request).
get(Name, Data) ->
	Defined = proplists:is_defined(Name, Data),
	case Defined of
		false ->
            throw({http_error_bad_request, iolist_to_binary([<<"Missing request variable: ">>,Name])});
		_ -> proplists:get_value(Name, Data)
	end.

%% @doc Returns value from request as a list
%% Failure: throw(http_error_bad_request).
get_list(Name, Data) -> asim_lib_utils_type_conv:to_list(get(Name, Data)).

%% @doc Returns value from request as an ASCII list
%% Notice the input comming from client could actually be any UTF8 unicode string.
%% So first we decode unicode then we check for invalid characters.
%% Failure: throw(http_error_bad_request).
get_ascii_list(Name, Data) ->

    List = get_unicode_list(Name, Data),
    case asim_lib_utils_unicode:is_ascii(List) of
        true -> List;
        _ -> throw({http_error_bad_request, iolist_to_binary([<<"Invalid ASCII string request variable: ">>,Name])})
    end.

%% @doc Returns value from request as an UNICODE list
%% Failure: throw(http_error_bad_request).
get_unicode_list(Name, Data) ->

    Value = get(Name, Data),

    %% Try decoding
    try unicode:characters_to_list(Value, utf8) of

        Decoded ->

            case erlang:is_list(Decoded) of
                true -> Decoded;
                _ -> throw({http_error_bad_request, iolist_to_binary([<<"Invalid UNICODE UTF8 string request variable: ">>,Name])})
            end

    catch

        _:_ ->

            throw({http_error_bad_request, iolist_to_binary([<<"Invalid UNICODE UTF8 string request variable: ">>,Name])})

    end.

%% @doc Returns value from request as a binary
%% Failure: throw(http_error_bad_request).
get_binary(Name, Data) -> asim_lib_utils_type_conv:to_binary(get(Name, Data)).

%% @doc Returns value from request as a integer
%% Failure: throw(http_error_bad_request).
get_integer(Name, Data) ->
	Converted = asim_lib_utils_type_conv:to_integer(get(Name, Data)),
	case Converted of
		badarg -> throw({http_error_bad_request, iolist_to_binary([<<"Invalid request variable ">>, Name, <<" = ">>, Data, <<" (integer expected)">>])});
		_ -> Converted
	end.

%% @doc Returns value from request as a float
%% Failure: throw(http_error_bad_request).
get_float(Name, Data) ->
	Converted = asim_lib_utils_type_conv:to_float(get(Name, Data)),
	case Converted of
		badarg -> throw({http_error_bad_request, iolist_to_binary([<<"Invalid request variable ">>, Name, <<" = ">>, Data, <<" (float expected)">>])});
		_ -> Converted
	end.

%%%%%%%%%%%%%%%%
%% ALWAYS GET %%
%%%%%%%%%%%%%%%%

%% @doc Returns value from request as a list
%% Failure: returns Default if request does not contains the specified value or the specified value is malformed.
always_get_list(Name, Data, Default) ->

	Defined = proplists:is_defined(Name, Data),
	case Defined of
		false -> Default;
		_ ->
			asim_lib_utils_type_conv:to_list(proplists:get_value(Name, Data, Default))
	end.

%% @doc Returns value from request as an ASCII list
%% Failure: returns Default if request does not contains the specified value or the specified value is malformed.
always_get_ascii_list(Name, Data, Default) ->

    Defined = proplists:is_defined(Name, Data),
    case Defined of
        false -> Default;
        _ ->
            List = asim_lib_utils_type_conv:to_list(get(Name, Data)),
            case asim_lib_utils_list:is_ascii_string(List) of
                true -> List;
                _ -> Default
            end
    end.

%% @doc Returns value from request as an UNICODE list
%% Failure: returns Default if request does not contains the specified value or the specified value is malformed.
always_get_unicode_list(Name, Data, Default) ->

    Defined = proplists:is_defined(Name, Data),
    case Defined of
        false -> Default;
        _ ->
            %% Try decoding
            try unicode:characters_to_list(get(Name, Data), utf8) of
                Decoded ->
                    case erlang:is_list(Decoded) of
                        true -> Decoded;
                        _ -> Default
                    end
            catch
                _:_ -> Default
            end
    end.

%% @doc Returns value from request as a binary
%% Failure: returns Default if request does not contains the specified value or the specified value is malformed.
always_get_binary(Name, Data, Default) ->
	Defined = proplists:is_defined(Name, Data),
	case Defined of
		false -> Default;
		_ ->
			asim_lib_utils_type_conv:to_binary(proplists:get_value(Name, Data, Default))
	end.

always_get_trimed_non_empty_binary(Name, Data, Default) ->
	case always_get_trimed_utf8_binary(Name, Data, Default) of
		<<>> -> Default;
		Any -> Any
	end.

%% @doc Returns value from request as an UTF8 binary. The function will trim the binary and check if valid UTF8 encoding is used.
%% Failure: returns Default if request does not contains the specified value or the specified value is malformed.
always_get_trimed_utf8_binary(Name, Data, Default) ->

    Defined = proplists:is_defined(Name, Data),
    case Defined of
        false -> Default;
        _ ->
            %% Try decoding
            try unicode:characters_to_list(get(Name, Data), utf8) of
                Decoded ->
                    case erlang:is_list(Decoded) of

                        true ->

                            Trimmed = asim_lib_utils_unicode:trim(Decoded),
                            unicode:characters_to_binary(Trimmed, unicode, utf8);

                        _ -> Default
                    end
            catch
                _:_ -> Default
            end
    end.

%% @doc Returns value from request as a integer
%% Failure: returns Default if request does not contains the specified value or the specified value is malformed.
always_get_integer(Name, Data, Default) ->
	Defined = proplists:is_defined(Name, Data),
	case Defined of
		false -> Default;
		_ ->
			Converted = asim_lib_utils_type_conv:to_integer(proplists:get_value(Name, Data, Default)),
			case Converted of
				badarg -> Default;
				_ -> Converted
			end
	end.

%% @doc Returns value from request as a integer boolean
%% Failure: returns Default if request does not contains the specified value or the specified value is malformed.
always_get_integer_boolean(Name, Data, Default) ->
    Defined = proplists:is_defined(Name, Data),
    case Defined of
        false -> Default;
        _ ->
            Converted = asim_lib_utils_type_conv:to_integer(proplists:get_value(Name, Data, Default)),
            case Converted of
                0 -> 0;
                1 -> 1;
                _ -> Default
            end
    end.

%% @doc Returns value from request as a float
%% Failure: returns Default if request does not contains the specified value or the specified value is malformed.
always_get_float(Name, Data, Default) ->
	Defined = proplists:is_defined(Name, Data),
	case Defined of
		false -> Default;
		_ ->
			Converted = asim_lib_utils_type_conv:to_float(proplists:get_value(Name, Data, Default)),
			case Converted of
				badarg -> Default;
				_ -> Converted
			end
	end.


