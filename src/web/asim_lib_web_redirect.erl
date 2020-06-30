%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_redirect).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([route/2]).
-export([route/3]).
-export([url/3]).

-define(REDIRECT_TITLE, <<"You are being redirected...">>).
-define(REDIRECT_HELP, <<"If you are not redirected automatically then ~s">>).
-define(REDIRECT_HELP_CLICK_HERE, <<"click here">>).
-define(REDIRECT_MSG_TIME, <<"5">>).

%% @doc Redirect to the specified admin extension
route(Msg, RouteState) -> route(Msg, #asim_route{}, RouteState).
route(Msg, Route = #asim_route{}, RouteState) ->

    Url = asim_lib_web_url:get(Route),
    url(Msg, Url, RouteState).

%% @doc Redirect to the specified url
url(<<>>, Url, RouteState) -> redirect_http(Url, RouteState);
url(Msg, Url, RouteState) when erlang:is_list(Msg) -> url(erlang:iolist_to_binary(Msg), Url, RouteState);
url(Msg, Url, RouteState) when erlang:is_binary(Msg) -> redirect_with_msg(Msg, Url, RouteState);
url(_Msg, Url, RouteState) -> redirect_http(Url, RouteState).

%% @doc Redirect to the specified route with message
redirect_with_msg(Msg, Url, #asim_state{req = Req, opts = Opts, content_type = <<"text/html">>}) ->

    SafeUrl                 = Url,
    SafeMsg                 = asim_lib_web_htmlentities:encode(Msg),

    RedirectHelpClickHere   = erlang:iolist_to_binary(["<a href=\"", SafeUrl, "\">",  asim_lib_web_htmlentities:encode(?REDIRECT_HELP_CLICK_HERE), "</a>"]),
    RedirectHelp            = io_lib:format(?REDIRECT_HELP, [RedirectHelpClickHere]),
    Body                    = erlang:iolist_to_binary([
"<!DOCTYPE html>
<html lang=en>
<html>
<head>
<meta http-equiv=\"Refresh\" content=\"", ?REDIRECT_MSG_TIME, "; url=", SafeUrl, "\" />
<title>",  asim_lib_web_htmlentities:encode(?REDIRECT_TITLE), "</title>
<style type=\"text/css\">
<!--
body {
font-family: Arial, Helvetica, sans-serif;
font-size: 12px;
color: #000000;
background-color: #FFFFFF;
padding: 0px;
margin-top: 20px;
margin-right: 0px;
margin-bottom: 0px;
margin-left: 0px;
}
a {
text-decoration:none;
color:#C92120;
}
a:hover {
text-decoration:underline;
}
h4 {
font-size: 14px;
color: #000000;
}
h5 {
font-size: 10px;
color: #808080;
}
div.redirect {
width: 400px;
text-align: center;
vertical-align: middle;
border: 1px solid #0055A0;
padding: 20px;
margin: 0 auto;
}
-->
</style>
</head>
<body>
<div class=\"redirect\">
<h4>", SafeMsg,"</h4>
<h5>", RedirectHelp, "</h5>
</div>
</body>
</html>"]),

	FinalReq = asim_lib_http_reply:respond(<<"text/html">>, 200, Req, [], Body),
    erlang:throw({redirect, {ok, FinalReq, Opts}}).

%% @doc Redirect to the specified route without msg (HTTP redirect)
redirect_http(Url, #asim_state{req = Req, opts = Opts, content_type = ContentType}) ->

	FinalReq = asim_lib_http_reply:respond(ContentType, 307, Req, [{<<"Location">>, Url}], <<>>),
    erlang:throw({redirect, {ok, FinalReq, Opts}}).
