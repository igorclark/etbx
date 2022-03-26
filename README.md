etbx
=====

Simple Erlang/macOS port of [ex_termbox](https://github.com/ndreynolds/ex_termbox). Uses the NIF code from that project to expose [termbox](https://code.google.com/archive/p/termbox/) functionality to Erlang. Lazy port-compiler-only build requires [termbox installed via brew](https://formulae.brew.sh/formula/termbox).

Build
-----

    $ rebar3 compile

Run
---

	$ rebar3 shell


Use
---

- Ensure `etbx` app is started in your release, create a `gen_server` to act as your TUI manager and have it call `etbx_event:subscribe/1`
- It'll receive `#etbx_event` records for every keyboard event as `handle_info/2` messages, and you can use `etbx_core` functions like `put_cell/1` (to send `#etbx_cell` records containing `#etbx_position` children), `clear/0` and `present/0`
- Various constants (`colors`, `attributes` etc) are defined in `etbx_constants`
- When you're done listening to keyboard events and want to drop back to the erlang shell, call `etbx_event:unsubscribe/1`
- If `etbx_event` terminates the session (which it does by default on receiving 3x `ctrl-c` within 500ms) you'll receive a `handle_info/2` message `etbx_session_end` at which point you can decide what to do

Example `etbx_core:put_cell/1` call:

```erlang
etbx_core:put_cell( #etbx_cell{
    position = #etbx_position{
        x = X,
        y = Y
    },
    ch = Char,
    fg = maps:get( yellow, etbx_constants:colors() ),
    bg = maps:get( default, etbx_constants:colors() )
} ),
etbx_core:present().
```
