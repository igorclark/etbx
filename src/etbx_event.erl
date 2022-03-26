-module( etbx_event ).
-author( "Igor Clark <igor@igorclark.net>" ).
-behaviour( gen_server ).

%% ------------------------------------------------------------------
%% application-wide macros
%% ------------------------------------------------------------------

-include( "etbx.hrl" ).

%% ------------------------------------------------------------------
%% module-scope macros
%% ------------------------------------------------------------------

-define( EXIT_EVENT_TUPLE, { 1, 0, 3, 0, 0, 0, 0, 0 } ). % ctrl-c
-define( EXIT_EVENTS_REQUIRED, 3 ).
-define( EXIT_EVENTS_REQUIRED_IN_MS, 500 ).

%% ------------------------------------------------------------------
%% module records
%% ------------------------------------------------------------------

-record(
	state, {
		status :: ready | polling,
		recipients :: sets:set(),
		exits :: non_neg_integer(),
		exit_timeout_ref:: erlang:timer()
	}
).

%% ------------------------------------------------------------------
%% API function exports
%% ------------------------------------------------------------------

-export( [
	subscribe/1,
	unsubscribe/1,
	stop/0
] ).

%% ------------------------------------------------------------------
%% gen_server function/callback exports
%% ------------------------------------------------------------------

-export( [
	start_link/0,
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
] ).

%% ------------------------------------------------------------------
%% API function definitions
%% ------------------------------------------------------------------

subscribe( SubscriberPid ) ->
	gen_server:call( ?MODULE, { subscribe, SubscriberPid } ).

unsubscribe( SubscriberPid ) ->
	gen_server:call( ?MODULE, { unsubscribe, SubscriberPid } ).

stop() ->
	gen_server:stop( ?MODULE ).

%% ------------------------------------------------------------------
%% gen_server function definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link( { local, ?MODULE }, ?MODULE, [], [] ).

init( [] ) ->
	process_flag( trap_exit, true ),
	State = #state{
		status = ready,
		recipients = sets:new(),
		exits = 0
	},
    { ok, State }.

%% ------------------------------------------------------------------

handle_call( { subscribe, _SubscriberPid } = Msg, _From, #state{ status = ready } = State ) ->

	Pid = self(),

	etbx_core:init(),

	case etbx_core:start_polling( Pid ) of

		{ ok, _Resource } ->
			handle_call( Msg, _From, State#state{ status = polling } );

		{ error, already_polling } ->
			ok = etbx_core:stop_polling(),
			{ ok, _SelfResource } = etbx_core:start_polling( Pid ),
			handle_call( Msg, _From, State#state{ status = polling } );

		{ error, Error } ->
			{ reply, { error, { etbx_core, Error } }, State }

	end;

handle_call( { subscribe, SubscriberPid }, _From, #state{ recipients = Recipients } = State ) ->
	NewRecipients = sets:add_element( SubscriberPid, Recipients ),
	NewState = State#state{
		recipients = NewRecipients
	},
	{ reply, ok, NewState };

handle_call( { unsubscribe, SubscriberPid }, _From, #state{ recipients = Recipients } = State ) ->

	UnsubscribedState = State#state{
		recipients = sets:del_element( SubscriberPid, Recipients )
	},

	MaybeShutdownState = case sets:size( UnsubscribedState#state.recipients ) of

		0 ->
			ok = etbx_core:stop_polling(),
			etbx_core:shutdown(),
			UnsubscribedState#state{
				status = ready
			};

		_ ->
			UnsubscribedState

	end,

	{ reply, ok, MaybeShutdownState };
	
handle_call( _Request, _From, State ) ->
    { reply, { ignored_by, ?MODULE, self() }, State }.

%% ------------------------------------------------------------------

handle_cast( _Msg, State ) ->
    { noreply, State }.

%% ------------------------------------------------------------------

handle_info( clear_exits, State ) ->
	NewState = State#state{
		exits = 0
	},
	{ noreply, NewState };

% first ctrl-c within EXIT_EVENTS_REQUIRED_IN_MS
handle_info( { event, ?EXIT_EVENT_TUPLE } = Msg, #state{ exits = 0 } = State ) ->

	NewTimerRef = erlang:send_after( ?EXIT_EVENTS_REQUIRED_IN_MS, self(), clear_exits ),

	IncrementedExitState = State#state{
		exits = 1,
		exit_timeout_ref = NewTimerRef 
	},

	% forward all key events until we quit
	[ Pid ! event_tuple_to_record( Msg ) || Pid <- sets:to_list( State#state.recipients ) ],

	{ noreply, IncrementedExitState };

% subsequent ctrl-c's within EXIT_EVENTS_REQUIRED_IN_MS
handle_info( { event, ?EXIT_EVENT_TUPLE } = Msg, #state{ exits = Exits } = State)
when Exits < ( ?EXIT_EVENTS_REQUIRED - 1 ) ->
	
	IncrementedExitState = State#state{
		exits = Exits + 1
	},

	% forward all key events until we quit
	[ Pid ! event_tuple_to_record( Msg ) || Pid <- sets:to_list( State#state.recipients ) ],

	{ noreply, IncrementedExitState };

% final (EXIT_EVENTS_REQUIRED) ctrl-c within EXIT_EVENTS_REQUIRED_IN_MS
handle_info(
	{ event, ?EXIT_EVENT_TUPLE },
	#state{ exits = ( ?EXIT_EVENTS_REQUIRED - 1 ), exit_timeout_ref = ExitTimeoutRef } = State
) ->

	case is_reference( ExitTimeoutRef ) of
		true -> erlang:cancel_timer( ExitTimeoutRef );
		_ -> ok
	end,

	[ Pid ! etbx_session_end || Pid <- sets:to_list( State#state.recipients ) ],
	etbx_core:stop_polling(),
	etbx_core:shutdown(),

	NewState = State#state{
		recipients = sets:new(),
		status = ready,
		exits = 0
	},

	{ noreply, NewState };

handle_info( { event, { _Type, _Mod, _Key, _Ch, _W, _H, _X, _Y } } = EventTuple, State ) ->
	EventRecord = event_tuple_to_record( EventTuple ),
	handle_info( EventRecord, State );

handle_info( #etbx_event{} = Event, #state{ recipients = Recipients } = State ) ->
	[ Pid ! Event || Pid <- sets:to_list( Recipients ) ],
	{ noreply, State };
	
handle_info( _Info, State ) ->
    { noreply, State }.

%% ------------------------------------------------------------------

terminate( _Reason, _State ) ->
	etbx_core:stop_polling(),
    ok.

code_change( _OldVsn, State, _Extra ) ->
    { ok, State }.

%% ------------------------------------------------------------------
%% internal function definitions
%% ------------------------------------------------------------------

event_tuple_to_record( { event, { Type, Mod, Key, Ch, W, H, X, Y } } ) ->
	#etbx_event{
		type = Type,
		mod = Mod,
		key = Key,
		ch = Ch,
		w = W,
		h = H,
		x = X,
		y = Y
	}.
