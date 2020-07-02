%%%-------------------------------------------------------------------
%%% @author Madalin Grigore-Enescu <madalingrigoreenescu@gmail.com>
%%% @doc
%%% Contains global definitions
%%% @end
%%%-------------------------------------------------------------------
-author("madalin").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Various webservice HTML constants
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(ASIM_WEB_SERVICE_TITLE, "An virtual economies SIMulator").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SIMULATIONS RELATED STRUCTURES/RECORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Gene pool size
-define(ASIM_PLAYER_GENES_POOL_SIZE, 6).

%% Gene pool
%% archaic gene (has no effect on player behavior)
-define(ASIM_PLAYER_GENE_ARCHAIC, 1).
%% miner gene (increase the mining skill)
-define(ASIM_PLAYER_GENE_MINER, 2).
%% trader gene (increase the trading skill)
-define(ASIM_PLAYER_GENE_TRADER, 3).
%% lazy gene (increase the laziness of the player)
-define(ASIM_PLAYER_GENE_LAZY, 4).
%% thief gene (increase the thief skill)
-define(ASIM_PLAYER_GENE_THIEF, 5).
%% hoarder gene (increase the predisposition of the player to store resources).
-define(ASIM_PLAYER_GENE_HOARDER, 6).

%% Record holding a simulation player
-record(asim_player, {
	genome = undefined :: undefined | list(),
	production = 0 :: float(),
	capital = 0 :: float(),
	taxes = 0 :: float(),
	fitness = 0 :: float()
}).

%% Record holding a new bunch of simulations (2 or more) specifications
-record(asim_simulation_specs, {
	how_many,
	sim_id,
	sim_template_id
}).

%% Record holding simulation rules
-record(asim_simulation_rules, {

	%% Simulation rule id
	id = undefined,

	%% Simulation rule name
	name = <<"">>,

	%% Stop cycle (use undefined for running the simulation forever until stopped manually by the user)
	stop_cycle = 100,

	%% Population count
	population_count = 100,

	%% Genome size
	genome_size = 8,

	%% Selection count (from population count)
	selection_count = 50,

	%% Fitness function type
	fitness_function = <<"profit_oriented">>,

	%% Profit redistribution function
	profit_redistribution_function = <<"meritocracy">>,

	%% C – is the cost of the simulation cycle
	%% (a real number in between 0 and 1 interval)
	c_cost_of_the_simulation = 0.2,

	%% P – is the capital profit multiplier
	%% (a real number in between 0 and 1 interval)
	p_capital_multiplier = 0.1,

  %% X – is the taxation percent from the total amount of energy produced by a player into a simulation cycle
	%% (a real number in between 0 and 1 interval).,
  x_taxation_percent = 0.18,

	%% M – is a constant representing the simulation energy obtained by a miner gene
	m_gene_miner_energy = 16,

	%% T – is a constant representing the simulation energy obtained by a trader gene
	t_gene_trader_energy = 4,

	%% H – is a constant representing the ability to hoard of a hoarder gene
	h_gene_hoarder_ability = 0.1,

	%% Z – is a constant representing the ability to steal of a thief gene
	z_gene_thief_ability = 0.1,

	time_created

}).

%% Record holding simulation data
-record(asim_simulation, {

	%% Simulation unique ID
	id,

	%% Simulation cycle count
	cycle = 0,

	%% Record holding simulation rules
	rules = undefined,

	%% Holds population
	population = undefined,

	%% the maximum taxes amount payed by a single player for the current simulation step
	max_player_taxes = 0,

  %% G – is the total amount of energy produced by all players before taxes. G is a quite a similar simplification of a country gross domestic product (GDP).
	g_energy_produced = 0,

  %% B - is the total amount of the taxes collected from all players activities. This is like a country revenue or budget but of course in a more simplified form.
	b_taxes_collected = 0,

  %% Simulation start time
	time_started,

	%% Simulation end time
	time_ended

}).

%% Record holding simulation history data
-record(asim_simulation_history, {

	%% Simulation unique ID
	id,

	%% Simulation cycle count
	cycle = 0,

	%% Holds population
	population = undefined,

	%% the maximum taxes amount payed by a single player for the current simulation step
	max_player_taxes = 0,

	%% G – is the total amount of energy produced by all players before taxes. G is a quite a similar simplification of a country gross domestic product (GDP).
	g_energy_produced = 0,

	%% B - is the total amount of the taxes collected from all players activities. This is like a country revenue or budget but of course in a more simplified form.
	b_taxes_collected = 0,

	%% History info time
	time_created

}).

%% ETS memory tables
-define(ASIM_TABLE_COUNTERS,        asim_table_counters).
-define(ASIM_TABLE_CONFIG,          asim_table_config).
-define(ASIM_TABLE_ASSETS_FILES,    asim_table_assets_files).
-define(ASIM_TABLE_ASSETS_TEMPLATES,asim_table_assets_templates).
-define(ASIM_TABLE_IMT_EXTENSIONS,	asim_table_imt_extensions).
-define(ASIM_TABLE_IMT_TYPES,		asim_table_imt_types).

%% ETS tables specifications
%% Tables are automatically created using this tables list
%% We can easily add a new table to the simulation by appending the
%% ETS table to this list
-define(ASIM_TABLES_ETS, [
	{?ASIM_TABLE_COUNTERS, [named_table, public]},
	{?ASIM_TABLE_CONFIG, [named_table, public]},
	{?ASIM_TABLE_ASSETS_FILES, [named_table, public]},
	{?ASIM_TABLE_ASSETS_TEMPLATES, [named_table, public]},
	{?ASIM_TABLE_IMT_EXTENSIONS, [named_table, public]},
	{?ASIM_TABLE_IMT_TYPES, [named_table, public]}]).

%% Mnesia tables specifications
%% Tables are automatically created using this tables list
%% We can easily add a new table to the simulation by appending the
%% MNESIA table to this list
-define(ASIM_TABLES_MNESIA, [
	{asim_simulation_rules, [
		{disc_copies, [erlang:node()]},
		{type, set},
		{attributes, record_info(fields, asim_simulation_rules)}
	]},
	{asim_simulation, [
		{disc_copies, [erlang:node()]},
		{type, set},
		{attributes, record_info(fields, asim_simulation)}
	]},
	{asim_simulation_history, [
		{disc_copies, [erlang:node()]},
		{type, bag},
		{attributes, record_info(fields, asim_simulation_history)}
	]}
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Supervision and processes tree options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Supervision tree defaults
-define(ASIM_DEFAULT_SUP_RESTART_INTENSITY, 10).
-define(ASIM_DEFAULT_SUP_RESTART_PERIOD, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Structures related to web service "CMS"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Some sort of very primitive ACL
%% Prevent access to any other module through request parameters, except the modules from the list.
%% Also prevent exhausting atom tables resources.
-define(ASIM_MODULES_LIST, [
	{<<"asim_ext_home_index">>, asim_ext_home_index},
	{<<"asim_ext_results_index">>, asim_ext_results_index}
]).

%% Admin route state
-record(asim_state, {

	%% Init time of this admin route
	handle_time                 = undefined,

	%% Content type expected by remote user agent
	content_type                = <<"text/html">>,

	%% Method invoked by remote user agent
	method                      = undefined,

	%% Remote user info
	remote_ip                   = undefined,
	remote_port                 = undefined,
	remote_user_agent           = undefined,

	%% Cowboy request object
	req                         = undefined,

	%% Options
	opts                        = undefined,

	%% Current requested path
	path                        = undefined,

	%% Current admin extension
	extension                   = undefined,

	%% Current admin section module
	extension_module            = undefined,

	%% Current admin module operation
	extension_module_operation  = undefined,

	%% Current admin module full name (binary)
	extension_module_name       = undefined,

	%% Params that must be perpetuated if set between all modules of current extension
	extension_keep_params       = [],

	%% Current admin request
	request                     = undefined,
	request_headers             = undefined,
	request_post                = undefined,
	request_get                 = undefined,
	request_cookies             = undefined,

	%% Session
	session_options             = undefined,
	session                     = undefined,

	%% Output buffer
	ob                          = undefined

}).

-type asim_state() :: #asim_state{}.

-record(asim_route, {
	url                 = undefined,
	extension           = undefined,
	module              = undefined,
	operation           = undefined,
	params              = undefined
}).

-record(asim_ob, {
	story               = <<>>,
	messages            = [],
	toolbar             = [],
	js                  = <<>>,
	js_ready            = <<>>,
	css                 = <<>>,
	variables           = []
}).

-record(asim_ob_message, {
	message = undefined,
	class   = undefined
}).

-record(asim_ob_toolbar_button, {
	id                  = undefined,
	href                = <<"#">>,
	img                 = <<>>,
	name                = <<>>,
	target              = undefined,
	js_ready            = undefined,
	js_confirm          = false,
	js_confirm_question = <<"Continue?">>
}).

-record(asim_html_form, {
	method              = <<"post">>,
	id                  = undefined,
	name                = undefined,
	class               = <<"quickform">>,
	div_class           = <<"form-container">>,
	encoding            = undefined,
	action              = undefined,
	hidden_fields       = [],
	toolbar_buttons     = [],
	content             = <<>>,
	js_ready            = <<>>
}).

-record(asim_table_nav, {
	id,
	form,
	table,
	record,
	columns,
	key,
	option_auto_key_filter,
	operations,
	data_rows,
	option_msg,
	request
}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Define some UNICODE related constants hardly used in our simulation
%% web services (Erlang is quite lacking unicode facilities and most unicode
%% community projects outhere are quite obscure)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(UNICODE_NUMERIC_LIST, [
	{48, 57}
]).

-define(UNICODE_ALPHABETIC_LIST, [
	{65,90},
	{97,122},
	170,181,186,
	{192,214},
	{216,246},
	{248,442},
	{444,659},
	{661,705},
	{710,721},
	{736,740},
	750,837,
	{891,893},
	902,
	{904,906},
	908,
	{910,929},
	{931,974},
	{976,1013},
	{1015,1153},
	{1162,1299},
	{1329,1366},
	1369,
	{1377,1415},
	{1456,1469},
	1471,
	{1473,1474},
	{1476,1477},
	1479,
	{1488,1514},
	{1520,1522},
	{1552,1557},
	{1569,1594},
	{1601,1623},
	{1625,1630},
	{1646,1647},
	{1649,1747},
	{1750,1756},
	{1761,1768},
	{1774,1775},
	{1786,1788},
	1791,1808,
	{1810,1855},
	{1869,1901},
	{1920,1968},
	1969,
	{1994,2026},
	{2036,2037},
	2042,
	{2305,2306},
	{2308,2361},
	{2366,2380},
	2384,
	{2392,2403},
	{2427,2431},
	{2434,2435},
	{2437,2444},
	{2447,2448},
	{2451,2472},
	{2474,2480},
	2482,
	{2486,2489},
	{2494,2500},
	{2503,2504},
	{2507,2508},
	2510,2519,
	{2524,2525},
	{2527,2531},
	{2544,2545},
	{2561,2563},
	{2565,2570},
	{2575,2576},
	{2579,2600},
	{2602,2608},
	{2610,2611},
	{2613,2614},
	{2616,2617},
	{2622,2626},
	{2631,2632},
	{2635,2636},
	{2649,2652},
	2654,
	{2672,2676},
	{2689,2690},
	2691,
	{2693,2701},
	{2703,2705},
	{2707,2728},
	{2730,2736},
	{2738,2739},
	{2741,2745},
	{2750,2757},
	{2759,2761},
	{2763,2764},
	2768,
	{2784,2787},
	{2818,2819},
	{2821,2828},
	{2831,2832},
	{2835,2856},
	{2858,2864},
	{2866,2867},
	{2869,2873},
	{2877,2879},
	{2881,2883},
	{2887,2888},
	{2891,2892},
	{2902,2903},
	{2908,2909},
	{2911,2913},
	2929,2946,2947,
	{2949,2954},
	{2958,2960},
	{2962,2965},
	{2969,2970},
	2972,
	{2974,2975},
	{2979,2980},
	{2984,2986},
	{2990,3001},
	{3006,3007},
	{3009,3010},
	{3014,3016},
	{3018,3020},
	3031,
	{3073,3075},
	{3077,3084},
	{3086,3088},
	{3090,3112},
	{3114,3123},
	{3125,3129},
	{3134,3140},
	{3142,3144},
	{3146,3148},
	{3157,3158},
	{3168,3169},
	{3202,3203},
	{3205,3212},
	{3214,3216},
	{3218,3240},
	{3242,3251},
	{3253,3257},
	3261,3262,
	{3264,3268},
	{3271,3272},
	{3274,3276},
	{3285,3286},
	3294,
	{3296,3299},
	{3330,3331},
	{3333,3340},
	{3342,3344},
	{3346,3368},
	{3370,3385},
	{3390,3395},
	{3398,3400},
	{3402,3404},
	3415,
	{3424,3425},
	{3458,3459},
	{3461,3478},
	{3482,3505},
	{3507,3515},
	3517,
	{3520,3526},
	{3535,3540},
	3542,
	{3544,3551},
	{3570,3571},
	{3585,3632},
	{3634,3642},
	{3648,3654},
	3661,
	{3713,3714},
	3716,
	{3719,3720},
	3722,3725,
	{3732,3735},
	{3737,3743},
	{3745,3747},
	3749,3751,
	{3754,3755},
	{3757,3760},
	{3762,3769},
	{3771,3773},
	{3776,3780},
	3782,3789,
	{3804,3805},
	3840,
	{3904,3911},
	{3913,3946},
	{3953,3966},
	{3968,3969},
	{3976,3979},
	{3984,3991},
	{3993,4028},
	{4096,4129},
	{4131,4135},
	{4137,4138},
	{4141,4146},
	4150,4152,
	{4176,4185},
	{4256,4293},
	{4304,4346},
	4348,
	{4352,4441},
	{4447,4514},
	{4520,4601},
	{4608,4680},
	{4682,4685},
	{4688,4694},
	4696,
	{4698,4701},
	{4704,4744},
	{4746,4749},
	{4752,4784},
	{4786,4789},
	{4792,4798},
	4800,
	{4802,4805},
	{4808,4822},
	{4824,4880},
	{4882,4885},
	{4888,4954},
	4959,
	{4992,5007},
	{5024,5108},
	{5121,5740},
	{5743,5750},
	{5761,5786},
	{5792,5866},
	{5870,5872},
	{5888,5900},
	{5902,5907},
	{5920,5939},
	{5952,5971},
	{5984,5996},
	{5998,6000},
	{6002,6003},
	{6016,6067},
	{6071,6085},
	{6087,6088},
	6103,6108,
	{6176,6210},
	{6212,6263},
	{6272,6313},
	{6400,6428},
	{6432,6443},
	{6448,6449},
	{6451,6456},
	{6480,6509},
	{6512,6516},
	{6528,6569},
	{6576,6601},
	{6656,6678},
	{6679,6683},
	{6912,6915},
	{6917,6963},
	{6966,6971},
	{6973,6979},
	{6981,6987},
	{7424,7543},
	{7545,7615},
	{7680,7835},
	{7840,7929},
	{7936,7957},
	{7960,7965},
	{7968,8005},
	{8008,8013},
	{8016,8023},
	8025,8027,8029,
	{8031,8061},
	{8064,8116},
	{8118,8124},
	8126,
	{8130,8132},
	{8134,8140},
	{8144,8147},
	{8150,8155},
	{8160,8172},
	{8178,8180},
	{8182,8188},
	8305,8319,
	{8336,8340},
	8450,8455,
	{8458,8467},
	8469,
	{8473,8477},
	8484,8486,8488,
	{8490,8493},
	{8495,8505},
	{8508,8511},
	{8517,8521},
	8526,
	{8544,8580},
	{9398,9449},
	{11264,11310},
	{11312,11358},
	{11360,11372},
	{11380,11383},
	{11392,11492},
	{11520,11557},
	{11568,11621},
	11631,
	{11648,11670},
	{11680,11686},
	{11688,11694},
	{11696,11702},
	{11704,11710},
	{11712,11718},
	{11720,11726},
	{11728,11734},
	{11736,11742},
	{12293,12295},
	{12321,12329},
	{12337,12341},
	{12344,12348},
	{12353,12438},
	{12445,12447},
	{12449,12538},
	{12540,12543},
	{12549,12588},
	{12593,12686},
	{12704,12727},
	{12784,12799},
	{13312,19893},
	{40960,40980},
	{40982,42124},
	{42775,42778},
	{43008,43009},
	{43011,43013},
	{43015,43018},
	{43020,43047},
	{43072,43123},
	{44032,55203},
	{63744,64045},
	{64048,64106},
	{64112,64217},
	{64256,64262},
	{64275,64279},
	64285,
	{64287,64296},
	{64298,64310},
	{64312,64316},
	64318,
	{64320,64321},
	{64323,64324},
	{64326,64433},
	{64467,64829},
	{64848,64911},
	{64914,64967},
	{65008,65019},
	{65136,65140},
	{65142,65276},
	{65313,65338},
	{65345,65370},
	{65382,65391},
	{65393,65470},
	{65474,65479},
	{65482,65487},
	{65490,65495},
	{65498,65500},
	{65536,65547},
	{65549,65574},
	{65576,65594},
	{65596,65597},
	{65599,65613},
	{65616,65629},
	{65664,65786},
	{65856,65908},
	{66304,66334},
	{66352,66368},
	{66370,66378},
	{66432,66461},
	{66464,66499},
	{66504,66511},
	{66513,66517},
	{66560,66717},
	{67584,67589},
	67592,
	{67594,67637},
	{67639,67640},
	67644,67647,
	{67840,67861},
	{68097,68099},
	{68101,68102},
	{68108,68115},
	{68117,68119},
	{68121,68147},
	{73728,74606},
	{74752,74850},
	{119808,119892},
	{119894,119964},
	{119966,119967},
	119970,
	{119973,119974},
	{119977,119980},
	{119982,119993},
	119995,
	{119997,120003},
	{120005,120069},
	{120071,120074},
	{120077,120084},
	{120086,120092},
	{120094,120121},
	{120123,120126},
	{120128,120132},
	120134,
	{120138,120144},
	{120146,120485},
	{120488,120512},
	{120514,120538},
	{120540,120570},
	{120572,120596},
	{120598,120628},
	{120630,120654},
	{120656,120686},
	{120688,120712},
	{120714,120744},
	{120746,120770},
	{120772,120779},
	{131072,173782},
	{194560,195101}]).

-define(UNICODE_WHITESPACE_CHARACTERS_LIST, [
	16#0009, %% character tabulation
	16#000A, %% line feed
	16#000B, %% line tabulation
	16#000C, %% form feed
	16#000D, %% carriage return
	16#0020, %% space Most common (normal ASCII space)
	16#0085, %% next line
	16#00A0, %% no-break space, identical to 16#0020, but not a point at which a line may be broken.
	16#1680, %% ogham space mark. Used for interword separation in Ogham text. Normally a vertical line in vertical text or a horizontal line in horizontal text, but may also be a blank space in "stemless" fonts.
	16#2000, %% en quad 8192 - Punctuation Separator space Width of one en.
	16#2001, %% em quad 8193 - Common General Punctuation Separator space Also known as "mutton quad". Width of one em.
	16#2002, %% en space 8194 - Common General Punctuation Separator space Also known as "nut". Width of one en.
	16#2003, %% em space 8195 - Common General Punctuation Separator space. Also known as "mutton". Width of one em.
	16#2004, %% three-per-em space 8196 - Common General Punctuation Separator space. Also known as "thick space". One third of an em wide.
	16#2005, %% four-per-em space 8197 - Common General Punctuation Separator space. Also known as "mid space". One fourth of an em wide.
	16#2006, %% six-per-em space 8198 - Common General Punctuation Separator space. One sixth of an em wide. In computer typography, sometimes equated to 16#2009.
	16#2007, %% figure space 8199 - Common General Punctuation Separator space. Figure space. In fonts with monospaced digits, equal to the width of one digit. HTML/XML named entity: &numsp;
	16#2008, %% punctuation space 8200 - Common General Punctuation Separator space. As wide as the narrow punctuation in a font, HTML/XML named entity: &puncsp;
	16#2009, %% thin space 8201 - Common General Punctuation Separator space. One-fifth (sometimes one-sixth) of an em wide. Recommended for use as a thousands separator for measures made with SI units.
	16#200A, %% hair space 8202 - Common General Punctuation Separator space. Thinner than a thin space. HTML/XML named entity: &hairsp;
	16#2028, %% line separator 8232 - Common General Punctuation Separator space. Line.
	16#2029, %% paragraph separator 8233 - Common General Punctuation Separator space. Paragraph.
	16#202F, %% narrow no-break space 8239 - Common General Punctuation Separator space. Narrow no-break space. Similar in function to 16#00A0 No-Break Space. When used with Mongolian, its width is usually one third of the normal space.
	16#205F, %% medium mathematical space 8287 - Common General Punctuation Separator space. MMSP. Used in mathematical formulae.
	16#3000  %% ideographic space 12288 - CJK Symbols Punctuation Separator, used, for example, in tai tou.
]).