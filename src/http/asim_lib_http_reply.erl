%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc Implements various useful functions to deal with an HTTP reply
%%% @end
%%%-------------------------------------------------------------------

-module(asim_lib_http_reply).

-export([respond_api_success/1]).
-export([respond_api_success/2]).
-export([respond_api_success/3]).
-export([respond_api_error/2]).
-export([respond_api_error/3]).
-export([respond_api_error/4]).
-export([respond_api_custom/4]).
-export([respond_api_custom_headers/4]).
-export([respond_api_custom_json/3]).

-export([respond/5]).

-export([respond_json/4]).
-export([respond_json/5]).
-export([respond_html/4]).
-export([respond_text_plain/4]).
-export([respond_xml/4]).
-export([respond_custom/4]).

%% Standard server headers
-define(XHTTP_STANDARD_HEADERS, [
    {<<"server">>, <<"asim">>}
    ]).

%% Standard API headers
-define(XHTTP_STANDARD_API_HEADERS, [
    {<<"Cache-Control">>, <<"no-store, no-cache, must-revalidate, post-check=0, pre-check=0">>},
    {<<"Pragma">>, <<"no-cache">>},
    {<<"connection">>, <<"close">>}
    ]).

%%%%%%%%%
%% API %%
%%%%%%%%%

%% @doc Respond with API standard error
respond_api_error(StatusCode,Req) ->
	respond_json(StatusCode,Req,?XHTTP_STANDARD_API_HEADERS,{[{<<"error">>, 1}]}).

respond_api_error(StatusCode,Req,ErrorDetails) ->
	respond_json(StatusCode,Req,?XHTTP_STANDARD_API_HEADERS,{[{<<"error">>, 1}, {<<"error_details">>, ErrorDetails}]}).

respond_api_error(StatusCode,Req,BodyData,ErrorDetails) ->
    Body = lists:append([{<<"error">>, 1}, {<<"error_details">>, ErrorDetails}],BodyData),
	respond_json(StatusCode,Req,?XHTTP_STANDARD_API_HEADERS,{Body}).

%% @doc Respond with API standard success
respond_api_success(Req) ->
    Body = [{<<"error">>, 0}],
    respond_json(200,Req,?XHTTP_STANDARD_API_HEADERS,{Body}).

respond_api_success(Req,BodyData) ->
    Body = lists:append([{<<"error">>, 0}],BodyData),
	respond_json(200,Req,?XHTTP_STANDARD_API_HEADERS,{Body}).

respond_api_success(Req, undefined, BodyData) ->
    Body = lists:append([{<<"session_key">>, <<"1">>}],BodyData),
	respond_api_success(Req,Body).

%% @doc Custom API response
respond_api_custom(StatusCode, Req, Headers, Body) ->

    StandardHeaders = headers_prepare(headers_add([?XHTTP_STANDARD_API_HEADERS, [{<<"content-type">>, <<"application/json">>}], Headers])),
    cowboy_reply(StatusCode, StandardHeaders, Body, Req).

%% @doc Custom API response
respond_api_custom_headers(StatusCode, Req, Headers, Body) ->

    cowboy_reply(StatusCode, Headers, Body, Req).

%% @doc Custom API response
respond_api_custom_json(StatusCode, Req, Body) ->

    respond_json(StatusCode,Req,?XHTTP_STANDARD_API_HEADERS,Body,false).

%%%%%%%%%%%%%
%% RESPOND %%
%%%%%%%%%%%%%

%% @doc Respond
%% This method will make sure to send the mandatory headers with the response.
respond(ContentType,StatusCode,Req,Headers,Body) ->
	StandardHeaders = headers_prepare(headers_add(Headers,[{<<"content-type">>, ContentType}])),
    cowboy_reply(StatusCode, StandardHeaders, Body, Req).

%% @doc Respond with json
respond_json(StatusCode,Req,Headers,Body) -> respond_json(StatusCode,Req,Headers,Body, true).
respond_json(StatusCode,Req,Headers,Body,true) ->
	JsonBody = xutils_lib_json:encode(Body),
    respond_json(StatusCode,Req,Headers,JsonBody,false);
respond_json(StatusCode,Req,Headers,Body,false) ->
    StandardHeaders = headers_prepare(headers_add(Headers,[{<<"content-type">>, <<"application/json">>}])),
    cowboy_reply(StatusCode, StandardHeaders, Body, Req).

%% @doc Respond with html
respond_html(StatusCode,Req,Headers,Body) ->
	StandardHeaders = headers_prepare(headers_add(Headers,[{<<"content-type">>, <<"text/html">>}])),
    cowboy_reply(StatusCode, StandardHeaders, Body, Req).

%% @doc Respond with text/plain
respond_text_plain(StatusCode,Req,Headers,Body) ->
	StandardHeaders = headers_prepare(headers_add(Headers,[{<<"content-type">>, <<"text/plain">>}])),
    cowboy_reply(StatusCode, StandardHeaders, Body, Req).

%% @doc Respond with XML
respond_xml(StatusCode,Req,Headers,Body) ->
	StandardHeaders = headers_prepare(headers_add(Headers,[{<<"content-type">>, <<"text/xml">>}])),
    cowboy_reply(StatusCode, StandardHeaders, Body, Req).

%% @doc Respond with custom everything
respond_custom(StatusCode, StandardHeaders, Body, Req) -> cowboy_reply(StatusCode, StandardHeaders, Body, Req).

%%%%%%%%%%%
%% UTILS %%
%%%%%%%%%%%

%% @doc Build headers appending some standard ones like Server header.
%% By default cowboy/mochiweb output his own server header and we don't want an attacker to know we are using cowboy or mochiweb.
headers_prepare([]) -> ?XHTTP_STANDARD_HEADERS;
headers_prepare(List) -> lists:append(List, ?XHTTP_STANDARD_HEADERS).

%% @doc Build headers appending some standard ones like Server header.
%% By default cowboy/mochiweb output his own server header and we don't want an attacker to know we are using cowboy or mochiweb.
headers_add([],Second) -> Second;
headers_add(First,Second) -> lists:append(First,Second).
headers_add(First) -> lists:append(First).


%% @doc Wrapper for cowboy_req:reply() with extra logging.
cowboy_reply(StatusCode, StandardHeaders, Body, Req) when erlang:is_list(StandardHeaders) ->

	cowboy_reply(StatusCode, maps:from_list(StandardHeaders), Body, Req);

cowboy_reply(StatusCode, StandardHeaders, Body, Req) when erlang:is_map(StandardHeaders) ->

    %% Log various information here if necessary
    case StatusCode of
        200 -> ignored;
        _ -> ignored
    end,

    %% Reply
    cowboy_req:reply(StatusCode, StandardHeaders, Body, Req).


