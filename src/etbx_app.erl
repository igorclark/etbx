%%%-------------------------------------------------------------------
%% @doc etbx public API
%% @end
%%%-------------------------------------------------------------------

-module( etbx_app ).

-behaviour( application ).

-export( [ start/2, stop/1 ] ).

start( _StartType, _StartArgs ) ->
    etbx_sup:start_link().

stop( _State ) ->
    ok.

%% internal functions
