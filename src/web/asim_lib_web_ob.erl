%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(asim_lib_web_ob).
-author("madalin").

-include("../include/asim.hrl").

%% API
-export([prepend_story/2]).
-export([prepend_message/2]).
-export([prepend_toolbar_button/2]).
-export([prepend_toolbar_buttons/2]).
-export([prepend_js/2]).
-export([prepend_jsready/2]).

-export([add_story/2]).
-export([add_story_operation_result/2]).
-export([add_message/2]).
-export([add_toolbar_button/2]).
-export([add_toolbar_buttons/2]).
-export([add_js/2]).
-export([add_jsready/2]).
-export([add_variables/2]).

-export([clear/1]).
-export([clear_story/1]).
-export([clear_js/1]).
-export([clear_jsready/1]).
-export([clear_variables/1]).

-export([render/1]).
-export([render_and_reply/1]).
-export([render_and_reply/3]).

-export([convert_result_to_html/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prepend
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Prepend the specified data to the body output buffer
prepend_story(Data, Ob = #asim_ob{story = OldOb}) ->

	Ob#asim_ob{story = erlang:iolist_to_binary([Data, OldOb])}.

%% @doc Prepend the specified data to the body output buffer
prepend_message(Message = #asim_ob_message{}, Ob = #asim_ob{messages = OldMessages}) ->

	Ob#asim_ob{messages = lists:append([Message, OldMessages])}.

%% @doc Prepend the specified data to the body output buffer
prepend_toolbar_button(ToolbarButton = #asim_ob_toolbar_button{}, Ob = #asim_ob{toolbar = Old}) ->

	Ob#asim_ob{toolbar = lists:append([ToolbarButton, Old])}.

%% @doc Prepend the specified toolbar buttons to the body output buffer
prepend_toolbar_buttons(ToolbarButtons, Ob = #asim_ob{toolbar = Old}) when erlang:is_list(ToolbarButtons) ->

	Ob#asim_ob{toolbar = lists:append(ToolbarButtons, Old)}.

%% @doc Prepend the specified data to the java script output buffer
prepend_js(Data, Ob = #asim_ob{js = OldOb}) ->

	Ob#asim_ob{js = erlang:iolist_to_binary([Data, OldOb])}.

%% @doc Prepend the specified data to the javascript ready output buffer
prepend_jsready(Data, Ob = #asim_ob{js_ready = OldOb}) ->

	Ob#asim_ob{js_ready = erlang:iolist_to_binary([Data, OldOb])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% add
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Add the specified data to the body output buffer
add_story(Data, Ob = #asim_ob{story = OldOb}) ->

	Ob#asim_ob{story = erlang:iolist_to_binary([OldOb, Data])}.

%% @doc Add story from operation result
add_story_operation_result(Result, Ob) ->

	Html = convert_result_to_html(Result),
	add_story(Html, Ob).

%% @doc Add the specified data to the body output buffer
add_message(Message = #asim_ob_message{}, Ob = #asim_ob{messages = OldMessages}) ->

	Ob#asim_ob{messages = lists:append(OldMessages, [Message])}.

%% @doc Add the specified data to the body output buffer
add_toolbar_button(ToolbarButton = #asim_ob_toolbar_button{}, Ob = #asim_ob{toolbar = Old}) ->

	Ob#asim_ob{toolbar = lists:append(Old, [ToolbarButton])}.

%% @doc Add the specified toolbar buttons to the body output buffer
add_toolbar_buttons(ToolbarButtons, Ob = #asim_ob{toolbar = Old}) when erlang:is_list(ToolbarButtons) ->

	Ob#asim_ob{toolbar = lists:append(Old, ToolbarButtons)}.

%% @doc Add the specified data to the java script output buffer
add_js(Data, Ob = #asim_ob{js = OldOb}) ->

	Ob#asim_ob{js = erlang:iolist_to_binary([OldOb, Data])}.

%% @doc Add the specified data to the javascript ready output buffer
add_jsready(Data, Ob = #asim_ob{js_ready = OldOb}) ->

	Ob#asim_ob{js_ready = erlang:iolist_to_binary([OldOb, Data])}.

%% @doc Add the specified template variables
add_variables(Data, Ob = #asim_ob{variables = OldOb}) ->

	Ob#asim_ob{variables = lists:append([OldOb, Data])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% clear
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Clear all content of output buffer
clear(_Ob = #asim_ob{}) -> #asim_ob{story = <<>>, js = <<>>, js_ready = <<>>}.

%% @doc Clear all content of output buffer
clear_story(Ob = #asim_ob{}) -> Ob#asim_ob{story = <<>>}.

%% @doc Clear all content of output buffer
clear_js(Ob = #asim_ob{}) -> Ob#asim_ob{js = <<>>}.

%% @doc Clear all content of output buffer
clear_jsready(Ob = #asim_ob{}) -> Ob#asim_ob{js_ready = <<>>}.

%% @doc Clear all content of template variables
clear_variables(Ob = #asim_ob{}) -> Ob#asim_ob{variables = []}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% render
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Render the content of output buffer

%% Render standard html template
render(State = #asim_state{content_type = <<"text/html">>,
	extension                  = Extension,
	extension_module           = ExtensionModule,
	extension_module_operation = ExtensionModuleOperation,
	ob = #asim_ob{
		story                  = Story,
		messages               = Messages,
		toolbar                = Toolbar,
		js                     = Js,
		js_ready               = JsReady,
		css                    = Css,
		variables              = TemplateVariables
	}}) ->

	{ProperToolbar, ToolbarJsReady}     = render_get_toolbar(Toolbar, JsReady),
	ProperMessages                      = render_get_messages(Messages),

	%% Render story first
	StoryTemplateName   = render_pick_template(true, Extension, ExtensionModule, ExtensionModuleOperation, <<"story">>),
	StoryBinary         = render_html_template(StoryTemplateName, lists:append(TemplateVariables, [{story, Story}]), State),

	%% Pick up the proper template name
	TemplateName        = render_pick_template(true, Extension, ExtensionModule, ExtensionModuleOperation, <<"page">>),

	%% Return final body
	render_html_template(TemplateName,
		lists:append([
			TemplateVariables,
			[
				{story, StoryBinary},
				{toolbar, ProperToolbar},
				{js, Js},
				{js_ready, ToolbarJsReady},
				{css, Css},
				{messages, ProperMessages}
			]
		]), State).

%% @doc Render the content of current output buffers and send http reply
render_and_reply(State) -> render_and_reply(200, [], State).
render_and_reply(HttpStatusCode, Headers, State = #asim_state{req = Req, opts = Opts}) ->

	Body = render(State),
	{ok, asim_lib_http_reply:respond_html(HttpStatusCode, Req, Headers, Body), Opts}.

%% @doc Render toolbar
render_get_toolbar(Toolbar, JsReadyAcum) -> render_get_toolbar(Toolbar, <<>>, JsReadyAcum).
render_get_toolbar([#asim_ob_toolbar_button{
	id                  = Id,
	href                = Href,
	img                 = Img,
	name                = Name,
	js_ready            = JsReady,
	js_confirm          = JsConfirm,
	js_confirm_question = JsConfirmQuestion,
	target              = Target
}|T], Acum, JsReadyAcum) ->

	ButtonId    = asim_lib_web_htmlentities:encode(asim_lib_web_html_id:is_valid_or_new(Id)),
	ButtonHref  = Href,
	ButtonImg   = Img,
	ButtonName  = asim_lib_web_htmlentities:encode(Name),
	ButtonTarget= Target,

	ButtonTargetBin = case ButtonTarget of
		                  undefined -> <<"">>;
		                  _ -> erlang:iolist_to_binary([<<"\" target=\"">>, ButtonTarget])
	                  end,

	case JsReady of
		undefined ->

			case JsConfirm of

				true ->

					NewAcum = erlang:iolist_to_binary([
						Acum,
						<<"\n<div class=\"button\"><a id=\"">>,
						ButtonId,
						<<"\" href=\"#">>,
						ButtonTargetBin,
						"\"><img src=\"",
						ButtonImg,
						"\" alt=\"",
						ButtonName,
						"\" /><span>",
						ButtonName,
						"</span></a></div>"
					]),

					render_get_toolbar(T, NewAcum,
						erlang:iolist_to_binary([
							JsReadyAcum,
							<<"\n$(\"#">>,
							ButtonId,
							<<"\").click(function() {\n">>,
							<<"\ngem_confirm(\"">>,
							asim_lib_web_htmlentities:encode(JsConfirmQuestion),
							<<"\", function() {\nwindow.location = '">>,
							ButtonHref,
							<<"';\n});">>,
							<<"\nreturn false;\n});">>
						]));

				_ ->

					NewAcum = erlang:iolist_to_binary([
						Acum,
						<<"\n<div class=\"button\"><a id=\"">>,
						ButtonId,
						<<"\" href=\"">>,
						ButtonHref,
						ButtonTargetBin,
						"\"><img src=\"",
						ButtonImg,
						"\" alt=\"",
						ButtonName,
						"\" /><span>",
						ButtonName,
						"</span></a></div>"
					]),

					render_get_toolbar(T, NewAcum, JsReadyAcum)

			end;

		_ ->

			NewAcum = erlang:iolist_to_binary([
				Acum,
				<<"\n<div class=\"button\"><a id=\"">>,
				ButtonId,
				<<"\" href=\"#">>,
				ButtonTargetBin,
				"\"><img src=\"",
				ButtonImg,
				"\" alt=\"",
				ButtonName,
				"\" /><span>",
				ButtonName,
				"</span></a></div>"
			]),

			case JsConfirm of

				true ->

					render_get_toolbar(T, NewAcum,
						erlang:iolist_to_binary([
							JsReadyAcum,
							<<"\n$(\"#">>,
							ButtonId,
							<<"\").click(function() {\n">>,
							<<"\ngem_confirm(\"">>,
							asim_lib_web_htmlentities:encode(JsConfirmQuestion),
							<<"\", function() {\n">>,
							JsReady,
							<<"\n});">>,
							<<"\nreturn false;\n});">>
						]));

				_ ->

					render_get_toolbar(T, NewAcum,
						erlang:iolist_to_binary([
							JsReadyAcum,
							<<"\n$(\"#">>,
							ButtonId,
							<<"\").click(function() {\n">>,
							JsReady,
							<<"\nreturn false;\n});">>
						]))
			end

	end;

render_get_toolbar([], Acum, JsReady) -> {Acum, JsReady}.

%% @doc Render messages
render_get_messages(Messages) -> render_get_messages(Messages, <<>>).
render_get_messages([#asim_ob_message{message = Message, class = Class}|T], Acum) ->
	Msg     = erlang:iolist_to_binary([<<"<div class=\"msg-">>, Class, <<"\">">>, asim_lib_web_htmlentities:encode(Message), <<"</div>">>]),
	NewAcum = <<Acum/binary, Msg/binary>>,
	render_get_messages(T, NewAcum);
render_get_messages([], Acum) -> Acum.

%% @doc Pick up the proper template
%% When custom parameter is true:
%% - First we check if custom template exist for current operation
%% - Then we check if custom template exist for current module
%% - Then we check if custom template exist for current extension
%% - If not we fallback using generic admin panel template
%% For optimization reasons those checks are made only if custom template parameter is true.
render_pick_template(false, _Extension, _ExtensionModule, _ExtensionModuleOperation, Fragment) ->
	erlang:iolist_to_binary([<<"/v1/html/">>, Fragment, <<".html">>]);
render_pick_template(_, Extension, ExtensionModule, ExtensionModuleOperation, Fragment) ->

	Template1 = erlang:iolist_to_binary([<<"/v1/html/">>, Extension, <<"/">>, ExtensionModule, <<"-">>, ExtensionModuleOperation, <<"-">>, Fragment, <<".html">>]),
	case asim_lib_utils_assets:template_exist(Template1) of
		true -> Template1;
		_ ->

			Template2 = erlang:iolist_to_binary([<<"/v1/html/">>, Extension, <<"/">>, ExtensionModule, <<"-">>, Fragment, <<".html">>]),
			case asim_lib_utils_assets:template_exist(Template2) of
				true -> Template2;
				_ ->

					Template3 = erlang:iolist_to_binary([<<"/v1/html/">>, Extension, <<"/">>, Fragment, <<".html">>]),
					case asim_lib_utils_assets:template_exist(Template3) of
						true -> Template3;
						_ -> erlang:iolist_to_binary([<<"/v1/html/">>, Fragment, <<".html">>])
					end
			end
	end.

%% @doc Render the specified html template
render_html_template(TemplateName, Params, #asim_state{
	handle_time                 = HandleTime,
	remote_ip                   = RemoteIp,
	remote_port                 = RemotePort,
	remote_user_agent           = RemoteUserAgent,
	extension                   = Extension,
	extension_module            = ExtensionModule,
	extension_module_operation  = ExtensionModuleOperation
}) ->

	Protocol       = asim_lib_utils_config:get_option(protocol),
	Host           = asim_lib_utils_config:get_option(host),
	Port           = erlang:integer_to_binary(asim_lib_utils_config:get_option(http_port)),

	%% Build base url
	Url = erlang:iolist_to_binary([Protocol, <<"://">>, Host, <<":">>, Port]),

	AssetsUrl       = erlang:iolist_to_binary([Url, <<"/assets">>]),
	TimePassed      = io_lib:format("~.6f", [(asim_lib_utils_time:milliseconds() - HandleTime)/1000]),
	TemplateData    = lists:append([
		[
			{title, erlang:iolist_to_binary([?ASIM_WEB_SERVICE_TITLE, <<" - ">>, Extension, <<" - ">>, ExtensionModule])},
			{url, Url},
			{assets_url, AssetsUrl},
			{current_extension, Extension},
			{current_module, ExtensionModule},
			{current_operation, ExtensionModuleOperation},
			{remote_ip, asim_lib_utils_inet:host_to_binary(RemoteIp)},
			{remote_port, asim_lib_utils_type_conv:to_binary(RemotePort)},
			{remote_user_agent, RemoteUserAgent},
			{page_generation_time, TimePassed}
		],
		Params
	]),

	%% Render
	RenderResult = asim_lib_utils_assets:template_render(TemplateName, TemplateData),
	case is_binary(RenderResult) of
		true -> RenderResult
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% convert_
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Convert operation result to html
convert_result_to_html(Result) ->

	ListResult      = io_lib:format("~p", [Result]),
	EncodedResult   = asim_lib_web_htmlentities:encode(ListResult),
	<<"\n<div class=\"operation-result\">", EncodedResult, "</div>">>.