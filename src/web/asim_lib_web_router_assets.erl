%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_router_assets).

-export([init/2]).
-export([terminate/3]).

%% @doc Init callback
init(Req, Opts) ->

    {Method,Req1} = asim_lib_http_request:method(Req),
    handle_method(Method, Req1, Opts).

%% @doc Terminate callback.
%%
%% This callback is strictly reserved for any required cleanup. You cannot send a response from this function. There is no other return value.
%% If you used the process dictionary, timers, monitors or may be receiving messages, then you can use this function to clean them up, as
%% Cowboy might reuse the process for the next keep-alive request.
%% Note that while this function may be called in a Websocket handler, it is generally not useful to do any clean up as the process terminates
%% immediately after calling this callback when using Websocket.
terminate(_Reason, _Req, _State) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal
%%%%%%%%%%%%%%

%% Allow only get method
handle_method(<<"GET">>,Req,Opts) ->

    {Path, Req1}   = asim_lib_http_request:path(Req),
    RandomUrl      = asim_lib_utils_type_conv:to_binary(asim_lib_utils_config:get_option(random_url, <<>>)),
	AssetsBaseUrl  = case RandomUrl of
						 <<>> -> erlang:iolist_to_binary([<<"/assets">>]);
		                 _ -> erlang:iolist_to_binary([<<"/">>, RandomUrl, <<"/assets">>])
	                 end,
    AssetsBaseUrlSize = erlang:size(AssetsBaseUrl),
    case Path of
        <<AssetsBaseUrl:AssetsBaseUrlSize/binary, AssetFile/binary>> ->
            AssetBinary = asim_lib_utils_assets:file_get(AssetFile),
            case AssetBinary of
                false ->
                    Req2 = asim_lib_http_reply:respond_custom(404, [{<<"content-type">>, <<"text/html">>}], <<"404 Not Found.">>, Req1),
                    {ok, Req2, Opts};
                _ ->
                    AssetIMT = asim_lib_web_imt:get_file_imt(AssetFile,<<"application/octet-stream">>),
                    Req2 = asim_lib_http_reply:respond_custom(200, [{<<"content-type">>, AssetIMT}], AssetBinary, Req1),
                    {ok, Req2, Opts}
            end;
        _ ->
            Req2 = asim_lib_http_reply:respond_custom(404, [{<<"content-type">>, <<"text/html">>}], <<"404 Not Found.">>, Req1),
            {ok, Req2, Opts}
    end;

%% Display proper HTTP response code for other invalid methods
handle_method(_,Req,Opts) ->

    Req2 = asim_lib_http_reply:respond_custom(404, [{<<"content-type">>, <<"text/html">>}], <<"404 Not Found.">>, Req),
    {ok, Req2, Opts}.



