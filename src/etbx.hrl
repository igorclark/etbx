-record(
	etbx_position, {
		x :: non_neg_integer(),
		y :: non_neg_integer()
	}
).

-record(
	etbx_cell, {
		position :: #etbx_position{},
		ch :: non_neg_integer(),
		fg :: non_neg_integer(),
		bg :: non_neg_integer()
	}
).

-record(
	etbx_event, {
		type :: non_neg_integer(),
		mod :: non_neg_integer(),
		key :: non_neg_integer(),
		ch :: non_neg_integer(),
		w :: non_neg_integer(),
		h :: non_neg_integer(),
		x :: non_neg_integer(),
		y :: non_neg_integer()
	}
).

