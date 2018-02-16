%%%-------------------------------------------------------------------
%%% @author andranat
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2017 5:28 PM
%%%-------------------------------------------------------------------
-author("andranat").

-record(verify_or, {
  rules = []
}).
-record(verify_and, {
  rules = []
}).

-record(verify, {
  key,
  %% except any lists in first level
  expected_val   = any :: term() | [term()],
  %% or list of types, all types in list checked with logic 'or'
  is_necessary   = false,
  expected_type  = any :: map %% map analog for obj | lsit for arr
  | list | atom | binary
  | bin_int | bin_float %% autocast to types
  | float | integer,
  expected_range = any ::  {integer()|infinity, integer()|infinity} | {max, integer()} | {min, integer()} | integer(), %% or list combined rules with logic 'or'
  regexp_check   = false :: false | [list()], %% list with strings considered regexps
%%  regexp_capt    = false :: true | false,
  %% works only for not complex types -> transform value to first expected type if it possible
%%  is_cached      = false, %% if saved automaticaly set to true - for speed improvement
  replace_val    = true,
  custom         = undefined %% undefined | fun/1
}).

%% allowed casts
%% list to map   and reverse
%% bin  to float and reverse
%% bin  to int   and reverse
%% atom to binary
