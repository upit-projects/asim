%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_url).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([get_base_url/0]).
-export([get_assets_base_url/0]).

-export([get_asset_url/1]).

-export([get/1]).
-export([get/2]).
-export([get/3]).
-export([get/4]).

-export([get_current_extension/2]).
-export([get_current_extension/3]).
-export([get_current_extension/4]).
-export([get_current_module/2]).
-export([get_current_module/3]).

%% @doc Returns base URL
get_base_url() ->

    Protocol   = asim_lib_utils_config:get_option(protocol),
    Host       = asim_lib_utils_config:get_option(host),
    Port       = erlang:integer_to_binary(asim_lib_utils_config:get_option(http_port)),
    erlang:iolist_to_binary([Protocol, <<"://">>, Host, <<":">>, Port]).

%% @doc Returns assets base URL
get_assets_base_url() -> erlang:iolist_to_binary([get_base_url(), "/assets"]).

%% @doc Returns URL for the specified asset
get_asset_url(Asset) -> erlang:iolist_to_binary([get_base_url(), "/assets", Asset]).

%% @doc Returns URL corresponding to the specified route
get(#asim_route{
    url         = undefined,
    extension   = undefined,
    module      = undefined,
    operation   = undefined,
    params      = undefined
}) -> get_base_url();

get(#asim_route{
    url         = Url,
    extension   = undefined,
    module      = undefined,
    operation   = undefined,
    params      = undefined
}) when erlang:is_binary(Url) -> Url;

get(#asim_route{
    url         = undefined,
    extension   = Extension,
    module      = undefined,
    operation   = undefined,
    params      = Params
}) when erlang:is_binary(Extension) ->

    add_params(erlang:iolist_to_binary([get_base_url(), "/", Extension]), Params);

get(#asim_route{
    url         = undefined,
    extension   = Extension,
    module      = Module,
    operation   = undefined,
    params      = Params
}) when erlang:is_binary(Extension),
    erlang:is_binary(Module) ->

    add_params(erlang:iolist_to_binary([get_base_url(), "/", Extension, "/", Module]), Params);

get(#asim_route{
    url         = undefined,
    extension   = Extension,
    module      = Module,
    operation   = Operation,
    params      = Params
}) when erlang:is_binary(Extension),
        erlang:is_binary(Module),
        erlang:is_binary(Operation) ->

    add_params(erlang:iolist_to_binary([get_base_url(), "/", Extension, "/", Module, "/", Operation]), Params).

%% @doc Returns URL corresponding to the specified module
get(Extension, Module) ->

    erlang:iolist_to_binary([get_base_url(),"/",Extension,"/",Module]).

%% @doc Returns URL corresponding to the specified module and operation
get(Extension, Module, Operation) ->

    erlang:iolist_to_binary([get_base_url(),"/",Extension,"/",Module,"/",Operation]).

%% @doc Returns URL corresponding to the specified module, operation and params
get(Extension, Module, Operation, Params) ->

    AdminUrl = get(Extension, Module, Operation),
    add_params(AdminUrl,Params).

%% @doc Returns current extension URL
get_current_extension(AdminRoute = #asim_route{}, State = #asim_state{extension = Extension}) ->
    asim_lib_web_url:get(AdminRoute#asim_route{extension = Extension});
get_current_extension(Module, State) -> get_current_extension(Module, <<"default">>, [], State).
get_current_extension(Module, Operation, State) -> get_current_extension(Module, Operation, [], State).
get_current_extension(Module, Operation, Params, #asim_state{extension = Extension}) ->
    get(Extension, Module, Operation, Params).

%% @doc Returns current module URL
get_current_module(AdminRoute = #asim_route{}, State = #asim_state{extension = Extension, extension_module = ExtensionModule}) ->
    asim_lib_web_url:get(AdminRoute#asim_route{extension = Extension, module = ExtensionModule});
get_current_module(Operation, State) -> get_current_module(Operation, [], State).
get_current_module(Operation, Params, #asim_state{extension = Extension, extension_module = Module}) ->
    get(Extension, Module, Operation, Params).

%% @doc Add params to the end of the specified url
add_params(AdminUrl, Params) when erlang:is_list(Params), Params =/= [] -> add_params(AdminUrl, <<"?">>, Params);
add_params(AdminUrl, undefined) -> AdminUrl;
add_params(AdminUrl, []) -> AdminUrl.
add_params(AdminUrl, _Separator, []) -> AdminUrl;
add_params(AdminUrl, Separator, [{Name, Value}|T]) ->
    NewAdminUrl = erlang:iolist_to_binary([AdminUrl, Separator, Name, <<"=">>, add_param_value(Value)]),
    add_params(NewAdminUrl, <<"&">>, T).

add_param_value(true) -> <<"1">>;
add_param_value(false) -> <<"0">>;
add_param_value(Value) when erlang:is_integer(Value) -> erlang:integer_to_list(Value);
add_param_value(Value) when erlang:is_binary(Value) -> erlang:binary_to_list(Value);
add_param_value(Value) when erlang:is_list(Value) -> erlang:list_to_binary(http_uri:encode(Value)).


