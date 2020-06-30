%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_router).
-author("madalin").

-include("../include/asim.hrl").

-export([init/2]).
-export([terminate/3]).

%% @doc Init callback
init(Req, Opts) ->

    try

	    %% Parse various data
	    {RequestCookies, ParsedCookiesReq}  = asim_lib_http_request:parse_cookies(Req),
	    {Method, MethodReq}                 = asim_lib_http_request:method(ParsedCookiesReq),
	    {PathBin, PathReq}                  = asim_lib_http_request:path(MethodReq),
	    {{RemoteIp, RemotePort}, PeerReq}   = asim_lib_http_request:peer(PathReq),
	    {RequestHeaders, HeadersReq}        = asim_lib_http_request:parse_headers(PeerReq),

	    Path        = erlang:binary_to_list(PathBin),
	    Useragent   = asim_lib_http_request:always_get_list(<<"user-agent">>, RequestHeaders, 0),

	    %% Check path
	    case path_check(Path) of

		    {ok, PathComponents} ->

			    %% Handle path components
			    {Extension, ExtensionModule, ExtensionModuleOperation} = handle_path_components(PathComponents),

			    %% Parse GET/POST
			    {RequestGet, ParsedGetReq}   = asim_lib_http_request:parse_qs(HeadersReq),

			    %% Detect multipart
			    HeaderContentType            = proplists:get_value(<<"content-type">>, RequestHeaders),
			    {RequestPost, ParsedPostReq} = case HeaderContentType of
				                                   <<"multipart/form-data", _/binary>> -> asim_lib_http_request_multipart:parse_post(ParsedGetReq);
				                                   _ -> asim_lib_http_request:parse_post(ParsedGetReq)
			                                   end,

			    %% Build final request (order of overwriting is MP/GET/POST)
			    Request = build_request(RequestGet, RequestPost),

			    %% Overwrite any extension/module/operation from path with the one from GET/POST
			    ActionExtension                 = asim_lib_http_request:always_get_binary(<<"xae">>, Request, Extension),
			    ActionExtensionModule           = asim_lib_http_request:always_get_binary(<<"xaem">>, Request, ExtensionModule),
			    ActionExtensionModuleOperation  = asim_lib_http_request:always_get_binary(<<"xaemo">>, Request, ExtensionModuleOperation),

			    %% Get content type from request
			    ContentType = asim_lib_http_request:always_get_binary(<<"xect">>, Request, <<"text/html">>),

			    %% Create admin state
			    AdminState = #asim_state{
				    handle_time                 = asim_lib_utils_time:milliseconds(),
				    content_type                = ContentType,
				    method                      = Method,
				    remote_ip                   = RemoteIp,
				    remote_port                 = RemotePort,
				    remote_user_agent           = Useragent,
				    req                         = ParsedPostReq,
				    opts                        = Opts,
				    path                        = Path,
				    extension                   = ActionExtension,
				    extension_module            = ActionExtensionModule,
				    extension_module_operation  = ActionExtensionModuleOperation,
				    request                     = Request,
				    request_headers             = RequestHeaders,
				    request_get                 = RequestGet,
				    request_post                = RequestPost,
				    request_cookies             = RequestCookies,
				    session                     = undefined,
				    session_options             = undefined,
				    ob                          = #asim_ob{}
			    },

			    %% Call the proper module according to request parameters
			    handle_module_call_function(AdminState);

		    _ ->

			    throw(http_error_not_found)

		end

	catch

	    throw:{redirect, Response} -> Response;

	    Class:Details ->

		    asim_lib_web_exceptions_handler:handle(Class, Details, Req, Opts)

	end.

%% @doc Terminate callback.
%%
%% This callback is strictly reserved for any required cleanup. You cannot send a response from this function. There is no other return value.
%% If you used the process dictionary, timers, monitors or may be receiving messages, then you can use this function to clean them up, as
%% Cowboy might reuse the process for the next keep-alive request.
%% Note that while this function may be called in a Websocket handler, it is generally not useful to do any clean up as the process terminates
%% immediately after calling this callback when using Websocket.
terminate(_Reason, _Req, _State) ->

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% call requested module function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Call the module and function requested by the route.
%% This function take care to rise proper exception for returning 404 Not found in the event the module and the route doesn't exist.
handle_module_call_function(AdminState = #asim_state{
	extension                   = ActionExtension,
	extension_module            = ActionExtensionModule,
	opts = Opts}) ->

	%% Get module name from string
	ModuleName      = erlang:iolist_to_binary([<<"asim_ext_">>, ActionExtension, <<"_">>, ActionExtensionModule]),
	AtomModuleName  = module_atom_name_from_binary(ModuleName),

    LoadResult = code:ensure_loaded(AtomModuleName),
    case LoadResult of
        {module, _} ->

            FinalResponse	= erlang:apply(AtomModuleName,handle,[AdminState]),
            case FinalResponse of
                {_,_,_} -> FinalResponse;
                _ -> {ok, FinalResponse, Opts}
            end;

        _ ->

            throw(http_error_not_found)

    end.

%% @doc Handle path components
%% URL are designed using the following schema: /[EXTENSION]/[MODULE]/[OPERATION]/
%% If [EXTENSION] is missing it will be populated with <<"home">> binary resulting in calling asim_ext_home_index:handle().
%% If [MODULE] is missing it will be populated with <<"index">> binary resulting in calling asim_ext_extension_index:handle().
%% If [OPERATION] is missing it will be populated with <<"default">> binary.
handle_path_components([]) -> {<<"home">>, <<"index">>, <<"default">>};
handle_path_components([Extension]) -> {Extension, <<"index">>, <<"default">>};
handle_path_components([Extension,Module]) -> {Extension, Module, <<"default">>};
handle_path_components([Extension,Module,Operation]) -> {Extension, Module, Operation};
handle_path_components(_) -> throw(not_found).

%% @doc Build request proplist from module parameters, GET and POST
build_request(Get, Post) ->

    OrddictGet      = orddict:from_list(Get),
    OrddictPost     = orddict:from_list(Post),
    MergeFun        = fun(_,_,Y) -> Y end,
    RequestOrddict  = orddict:merge(MergeFun, OrddictGet, OrddictPost),
    orddict:to_list(RequestOrddict).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Path check
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec
%% @doc Check path if does comply with our admin routes rules
path_check(Path) ->
    case string:str(Path, "//") of
        0 ->

            PathComponents = string:tokens(Path, "/"),
	        case path_check_optional_random_url(PathComponents) of

		        {ok, TailPathComponents} ->

			        case path_check_components(TailPathComponents) of
				        {ok, Acum} -> {ok, Acum};
				        error -> error
			        end;

				_ -> error

	        end;

        _ -> error
    end.

%% @doc Check optional random URL
path_check_optional_random_url(Path) ->

	RandomUrl = erlang:binary_to_list(asim_lib_utils_config:get_option(random_url, <<>>)),
	path_check_optional_random_url(RandomUrl, Path).

path_check_optional_random_url([], Tail) -> {ok, Tail};
path_check_optional_random_url(RandomUrl, [RandomUrl, Tail]) -> {ok, Tail};
path_check_optional_random_url(_, _) -> error.

%% @doc Check if all path components are composed of alphanumeric characters.
path_check_components(Components) -> path_check_components(Components, []).
path_check_components([H|T], Acum) ->
    case length(H) of
        0 -> error;
        _ ->
            case path_check_component(H) of
                ok ->

                    NewAcum = lists:append(Acum, [erlang:list_to_binary(H)]),
                    path_check_components(T, NewAcum);

                error -> error
            end
    end;
path_check_components([], Acum) -> {ok, Acum}.

%% @doc Check if path component is composed of alphanumeric characters.
path_check_component([Char|Rest]) when Char >= $a, Char =< $z -> path_check_component(Rest);
path_check_component([Char|Rest]) when Char >= $A, Char =< $Z -> path_check_component(Rest);
path_check_component([Char|Rest]) when Char >= $0, Char =< $9 -> path_check_component(Rest);
path_check_component([Char|Rest]) when Char == $_; Char == $- -> path_check_component(Rest);
path_check_component([]) -> ok;
path_check_component(_) -> error.

%% @doc Returns module atom name.
%% We use a predefined modules list to prevent injecting our system and exhausting atom creation limits
module_atom_name_from_binary(Binary) -> erlang:binary_to_atom(Binary, latin1);
module_atom_name_from_binary(Binary) ->

    case proplists:get_value(Binary, ?ASIM_MODULES_LIST, undefined) of
        undefined -> throw(http_error_not_found);
        Value -> Value
    end.

















