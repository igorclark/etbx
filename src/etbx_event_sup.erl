-module( etbx_event_sup ).
-author( "Igor Clark <igor@igorclark.net>" ).
-behaviour( supervisor ).

%% ------------------------------------------------------------------
%% application-wide macros
%% ------------------------------------------------------------------

%% ------------------------------------------------------------------
%% module-scope macros
%% ------------------------------------------------------------------

-define( SUPERVISOR_MODULE, ?MODULE ).
-define( WORKER_MODULE, etbx_event ).

%% ------------------------------------------------------------------
%% API function exports
%% ------------------------------------------------------------------

-export( [
] ).

%% ------------------------------------------------------------------
%% supervisor function/callback exports
%% ------------------------------------------------------------------

-export( [
	start_link/0,
	init/1
] ).

%% ------------------------------------------------------------------
%% API function definitions
%% ------------------------------------------------------------------

start_link() ->

	Name = { local, ?MODULE },
	{ ok, _Pid } = Link = supervisor:start_link( Name, ?MODULE, [] ),
	Link.

%% ------------------------------------------------------------------
%% supervisor function definitions
%% ------------------------------------------------------------------

init( [] ) ->

	ChildSpec = {
		?WORKER_MODULE,						% Id to register
		{ ?WORKER_MODULE, start_link, [] },	% M/F/A to spawn
		permanent,							% child restart type
		5000,								% child shutdown type
		worker,								% child type
		[ ?WORKER_MODULE ]					% list of involved modules, for hot code swaps
	},

	Children = [
		ChildSpec
	],

	RestartStrategy = {
		one_for_one,	% spawn new processes with this child spec on request
		5,				% no more than this many restarts in ...
		5				% ... this many seconds
	},

	{ ok, { RestartStrategy, Children } }.
