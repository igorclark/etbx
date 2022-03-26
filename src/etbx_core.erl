-module( etbx_core ).
-author( "Igor Clark <igor@igorclark.net>" ).

-on_load( load_nif/0 ).

-include( "etbx.hrl" ).

-export( [
	load_nif/0
] ).

-export( [
	init/0,
	width/0,
	height/0,
	clear/0,
	set_clear_attributes/2,
	present/0,
	set_cursor/2,
	put_cell/1,
	change_cell/5,
	select_input_mode/1,
	select_output_mode/1,
	start_polling/1,
	stop_polling/0,
	shutdown/0
] ).

load_nif() ->
	SoName = filename:join( code:priv_dir( etbx ), ?MODULE ),
	erlang:load_nif( SoName, 0 ).

-spec init() -> ok | { error, integer() | already_running }.
init() ->
	exit( nif_library_not_loaded ).

-spec width() -> { ok, integer() } | { error, not_running }.
width() ->
	exit( nif_library_not_loaded ).

-spec height() -> { ok, integer() } | { error, not_running }.
height() ->
	exit( nif_library_not_loaded ).

-spec clear() -> ok | { error, not_running }.
clear() ->
	exit( nif_library_not_loaded ).

-spec set_clear_attributes( etbx_constants:color(), etbx_constants:color() ) -> ok | { error, not_running }.
set_clear_attributes( _Fg, _Bg ) ->
	exit( nif_library_not_loaded ).

-spec present() -> ok | { error, not_running }.
present() ->
	exit( nif_library_not_loaded ).

-spec set_cursor( non_neg_integer(), non_neg_integer() ) -> ok | { error, not_running }.
set_cursor( _X, _Y ) ->
	exit( nif_library_not_loaded ).

-spec put_cell( #etbx_cell{} ) -> ok | { error, not_running }.
put_cell( #etbx_cell{ position = #etbx_position{ x = X, y = Y }, ch = Ch, fg = Fg, bg = Bg } ) ->
	change_cell( X, Y, Ch, Fg, Bg ).

-spec change_cell(
	  non_neg_integer(),
	  non_neg_integer(),
	  non_neg_integer(),
	  etbx_constants:color(),
	  etbx_constants:color()
	) -> ok | { error, not_running }.
change_cell( _X, _Y, _Ch, _Fg, _Bg ) ->
	exit( nif_library_not_loaded ).

-spec select_input_mode( etbx_constants:input_mode() ) ->  { ok, integer() } | { error, not_running }.
select_input_mode( _InputMode ) ->
	exit( nif_library_not_loaded ).

-spec select_output_mode( etbx_constants:output_mode() ) -> { ok, integer() } | { error, not_running }.
select_output_mode( _OutputMode ) ->
	exit( nif_library_not_loaded ).

-spec start_polling( pid() ) -> { ok, reference() } | { error, not_running | already_polling }.
start_polling( _Pid ) ->
	exit( nif_library_not_loaded ).

-spec stop_polling() -> ok | { error, not_running | not_polling }.
stop_polling() ->
	exit( nif_library_not_loaded ).

-spec shutdown() -> ok | { error, integer() | not_running }.
shutdown() ->
	exit( nif_library_not_loaded ).
