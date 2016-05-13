-module(mod_autosubscription).

-author('unixway.drive+ejabberd@gmail.com').

-behaviour(gen_mod).

-export([
	start/2,
	stop/1
]).

start(_Host, _Opts) ->
	ok.

stop(_Host) ->
	ok.
