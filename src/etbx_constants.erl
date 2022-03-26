-module( etbx_constants ).
-author( "Igor Clark <igor@igorclark.net>" ).

-export_type( [
	constant/0,
	key/0, color/0, attribute/0, event_type/0,
	error_code/0, input_mode/0, output_mode/0,
	hide_cursor/0
] ).
  
-type constant() :: integer.
-type key() :: constant.
-type color() :: constant.
-type attribute() :: constant.
-type event_type() :: constant.
-type error_code() :: constant.
-type input_mode() :: constant.
-type output_mode() :: constant.
-type hide_cursor() :: constant.

-export( [
	keys/0, colors/0, attributes/0, event_types/0,
	error_codes/0, input_modes/0, output_modes/0,
	hide_cursor/0
] ).

keys() -> #{
	arrow_down => 65516, ctrl_i => 9, mouse_wheel_up => 65509,
	ctrl_lsq_bracket => 27, f2 => 65534, ctrl_m => 13, f9 => 65527,
	ctrl_backslash => 28, ctrl_slash => 31, ctrl_7 => 31, arrow_left => 65515,
	ctrl_x => 24, ctrl_h => 8, ctrl_q => 17, ctrl_5 => 29, delete => 65522,
	ctrl_f => 6, ctrl_o => 15, ctrl_u => 21, ctrl_k => 11, ctrl_y => 25, tab => 9,
	home => 65521, ctrl_tilde => 0, space => 32, ctrl_3 => 27, f12 => 65524,
	f10 => 65526, arrow_up => 65517, ctrl_e => 5, ctrl_6 => 30, pgup => 65519,
	mouse_left => 65513, ctrl_g => 7, ctrl_p => 16, ctrl_8 => 127, ctrl_4 => 28,
	backspace2 => 127, ctrl_rsq_bracket => 29, 'end' => 65520, ctrl_j => 10,
	f11 => 65525, insert => 65523, mouse_right => 65512, enter => 13, pgdn => 65518,
	esc => 27, ctrl_n => 14, f4 => 65532, f6 => 65530, mouse_wheel_down => 65508,
	f5 => 65531, f8 => 65528, ctrl_a => 1, mouse_middle => 65511, ctrl_r => 18,
	arrow_right => 65514, f1 => 65535, f7 => 65529, mouse_release => 65510,
	ctrl_underscore => 31, ctrl_v => 22, ctrl_w => 23, ctrl_l => 12, ctrl_t => 20,
	ctrl_d => 4, backspace => 8, ctrl_b => 2, ctrl_s => 19, f3 => 65533,
	ctrl_z => 26, ctrl_c => 3, ctrl_2 => 0
}.

colors() -> #{
	black => 1, blue => 5, cyan => 7, default => 0, green => 3, magenta => 6,
	red => 2, white => 8, yellow => 4
}.

attributes() -> #{
	bold => 256, reverse => 1024, underline => 512
}.

event_types() -> #{
	key => 1, mouse => 3, resize => 2
}.

error_codes() -> #{
	failed_to_open_tty => -2, pipe_trap_error => -3, unsupported_terminal => -1
}.

input_modes() -> #{
	alt => 2, alt_with_mouse => 6, current => 0, esc => 1, esc_with_mouse => 5,
	mouse => 4
}.

output_modes() -> #{
	current => 0, grayscale => 4, normal => 1, term_216 => 3, term_256 => 2
}.

hide_cursor() -> -1.
