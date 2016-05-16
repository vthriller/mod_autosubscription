-module(mod_autosubscription).

-author('unixway.drive+ejabberd@gmail.com').

-behaviour(gen_mod).

-export([
	start/2,
	stop/1,
	get/2,
	get_jid_info/4,
	in_subscription/6,
	%out_subscription/4,
	get_subscription_lists/3,
	process_item/2
]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").
-include("logger.hrl").



start(Host, _Opts) ->
	ejabberd_hooks:add(roster_get,	Host,	?MODULE,	get, 120),
	ejabberd_hooks:add(roster_get_jid_info,	Host,	?MODULE,	get_jid_info, 120),
	ejabberd_hooks:add(roster_in_subscription,	Host,	?MODULE,	in_subscription, 20),
	%ejabberd_hooks:add(roster_out_subscription,	Host,	?MODULE,	out_subscription, 20),
	ejabberd_hooks:add(roster_get_subscription_lists,	Host,	?MODULE,	get_subscription_lists, 120),
	ejabberd_hooks:add(roster_process_item,	Host,	?MODULE,	process_item, 120).

stop(Host) ->
	ejabberd_hooks:delete(roster_get,	Host,	?MODULE,	get, 120),
	ejabberd_hooks:delete(roster_get_jid_info,	Host,	?MODULE,	get_jid_info, 120),
	ejabberd_hooks:delete(roster_in_subscription,	Host,	?MODULE,	in_subscription, 20),
	%ejabberd_hooks:delete(roster_out_subscription,	Host,	?MODULE,	out_subscription, 20),
	ejabberd_hooks:delete(roster_get_subscription_lists,	Host,	?MODULE,	get_subscription_lists, 120),
	ejabberd_hooks:delete(roster_process_item,	Host,	?MODULE,	process_item, 120).


get(Items, _US) ->
	?DEBUG("mod_autosubscription/get: ~p", [Items]),
	[Item#roster{
		subscription = new_subscription(Item#roster.jid, Item#roster.subscription),
		ask = new_ask(Item#roster.jid, Item#roster.ask)
	} || Item <- Items].

get_jid_info({Subscription, Groups}, _User, _Server, JID) ->
	?DEBUG("mod_autosubscription/get_jid_info: ~p ~p", [Subscription, JID]),
	{new_subscription(JID, Subscription), Groups}.

in_subscription(Acc, User, Server, From, _Type, _Reason) ->
	?DEBUG("mod_autosubscription/in_subscription: ~p ~p ~p ~p", [Acc, User, Server, From]),
	To = #jid{user = User, server = Server},
	case is_local(To) andalso is_local(From) of
		true -> {stop, false};
		false -> Acc
	end.

%out_subscription(User, Server, JID, Type) ->
%	ok.

get_subscription_lists({From, To}, _User, _Server) ->
	{
		lists:usort(From ++ local_users()),
		lists:usort(To   ++ local_users())
	}.

process_item(RosterItem, _Host) ->
	?DEBUG("mod_autosubscription/process_item: ~p", [RosterItem]),
	% XXX process roster.us as well?
	case RosterItem#roster.subscription =/= remove andalso is_local(RosterItem#roster.jid) of
		true -> RosterItem#roster{subscription = both, ask = none};
		false -> RosterItem
	end.



new_subscription(JID, S) ->
	case is_local(JID) of
		true -> both;
		false -> S
	end.

new_ask(JID, A) ->
	case is_local(JID) of
		true -> none;
		false -> A
	end.

is_local(#jid{server = Host}) ->
	lists:member(Host, ?MYHOSTS);
is_local({_, Host, _}) ->
	lists:member(Host, ?MYHOSTS).

local_users() ->
	lists:flatten([
		[{User, Server, <<"">>} || {User, Server} <- ejabberd_auth:get_vh_registered_users(Host)]
	|| Host <- ?MYHOSTS]).
