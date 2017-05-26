-module(ever_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	{ok, self()}.

stop(_State) ->
	ok.
