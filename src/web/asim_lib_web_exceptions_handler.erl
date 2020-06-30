%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_exceptions_handler).
-author("madalin").

-export([handle/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% handle
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================
%% throw
%%==============================================================

%%------------------------
%% 400
%%------------------------

handle(throw, {http_error_bad_request, Msg}, Req, Opts) ->

    respond_error(true, 400, Req, Opts, "bad_request", Msg);

%% @doc Handle throw:http_error_bad_request
handle(throw, http_error_bad_request, Req, Opts) ->

    respond_error(true, 400, Req, Opts, "bad_request", "Bad request");

%%------------------------
%% 403 Forbidden
%%------------------------

%% @doc throw:http_error_forbidden
handle(throw, http_error_forbidden, Req, Opts) ->

    respond_error(true, 403, Req, Opts, "http_error_forbidden", "Forbidden");

%%------------------------
%% 404 Not Found
%%------------------------

%% @doc throw:http_error_not_found
handle(throw, http_error_not_found, Req, Opts) ->

    respond_error(true, 404, Req, Opts, "not_found", "Not found");

%%------------------------
%% 405 Method Not Allowed
%%------------------------

%% @doc throw:http_error_method_not_allowed
handle(throw, http_error_method_not_allowed, Req, Opts) ->

    respond_error(true, 405, Req, Opts, "http_error_method_not_allowed", "Method not allowed");

%%------------------------
%% 406 Not Acceptable
%%------------------------

%% @doc throw:http_error_not_acceptable
handle(throw, http_error_not_acceptable, Req, Opts) ->

    respond_error(true, 406, Req, Opts, "http_error_not_acceptable", "Not acceptable");

%%------------------------
%% 408 Request Timeout
%%------------------------

%% @doc throw:http_error_request_timeout
handle(throw, http_error_request_timeout, Req, Opts) ->

    respond_error(true, 408, Req, Opts, "http_error_request_timeout", "Request timeout");

%%==============================================================
%% exit/error/others
%%==============================================================

%% @doc 500 internal server error
handle(Class, Reason, Req, Opts) ->

	StackTrace = erlang:get_stacktrace(),
    respond_error(true, 500, Req, Opts, "internal_server_error", {Class, Reason, StackTrace}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Respond error
respond_error(true, HttpCode, Req, Opts, ErrorCode, ErrorMessage) ->

    %% Log
    asim_lib_utils_log:error(<<"asim">>, <<"http_error">>, [{error_http_code, HttpCode}, {error_code, ErrorCode}, {error_msg, ErrorMessage}]),

    %% Try to get content type
    ContentType = respond_error_always_get_content_type(Req),

    %% Respond
    case ContentType of
        <<"text/html">> ->

            Body = erlang:iolist_to_binary([asim_lib_utils_type_conv:to_binary(HttpCode), <<" - ">>, message_to_binary(ErrorMessage)]),
            {ok, asim_lib_http_reply:respond_html(HttpCode, Req, [], Body), Opts};

        _ -> {ok, <<>>, Opts}
    end.

%% @doc Respond error always get expected content type
respond_error_always_get_content_type(Req) ->

    try

        %% Get content type from GET
        {RequestGet, ParsedGetReq} = asim_lib_http_request:parse_qs(Req),
        GetContentType = asim_lib_http_request:always_get_binary(<<"xect">>, RequestGet, undefined),
        case GetContentType of
            undefined ->

                %% Get content type from POST
                {RequestHeaders, ParsedHeadersReq}  = asim_lib_http_request:parse_headers(ParsedGetReq),
                %% Detect multipart
                HeaderContentType            = proplists:get_value(<<"content-type">>, RequestHeaders),
                {RequestPost, _ParsedPostReq}= case HeaderContentType of
                                                   <<"multipart/form-data", _/binary>> -> asim_lib_http_request_multipart:parse_post(ParsedHeadersReq);
                                                   _ -> asim_lib_http_request:parse_post(ParsedHeadersReq)
                                               end,
                asim_lib_http_request:always_get_binary(<<"xect">>, RequestPost, <<"text/html">>);

            _ -> GetContentType

        end

    catch

        _:_ -> <<"text/html">>

    end.

message_to_binary(Any) when is_binary(Any) -> Any;
message_to_binary(Any) when is_integer(Any) -> message_to_binary(integer_to_list(Any));
message_to_binary(Any) when is_float(Any) -> float_to_binary(Any);
message_to_binary(Any) when is_atom(Any) -> atom_to_binary(Any,utf8);
message_to_binary(Any) -> io_lib:format("~p", [Any]).

