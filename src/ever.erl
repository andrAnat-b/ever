%%%-------------------------------------------------------------------
%%% @author andranat
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. May 2017 4:38 PM
%%%-------------------------------------------------------------------
-module(ever).
-author("andranat").
-include("ever_h.hrl").
%%-compile({no_auto_import, [get/2]}).
%% API
-export([chk/2]).

%% Service API & Utilites
-export([get/2, get/3, set/3, set/4, get_vpath/0]).

get_vpath() ->
  [getd('$$c_key'), getd('$_or_err'), getd('$validation_errors')].

-spec chk (Rule :: tuple(), Object :: map()) -> {true, Obj :: map()} | {false, Err :: any()}.
chk(Rule, Object) when is_tuple(Rule)->
  check(Rule, Object).

%% Inner BLOCK
check(#verify_and{rules = Rules}, Object) when is_list(Rules) ->
  check_and(Rules, Object, true);
check(#verify_or{rules = Rules}, Object)  when is_list(Rules) ->
  check_or(Rules, Object, []);
check(#verify{key = Key} = Rule, Object) when Key =/= undefined ->
         _ = setd('$$c_key', Key),
  Value    = {true, value, get(Key, Object, '$not_found!')},
  % io:format("\e[95m[dbg]\e[0m value [~p] \e[35m~p\e[0m\n", [?LINE, Value]),
  CNResult = check_necessary(Key,      Value,      Rule#verify.is_necessary),
  % io:format("\e[95m[dbg]\e[0m check_necessary [~p] \e[35m~p\e[0m\n", [?LINE, CNResult]),
  CEResult = check_expected(Key,       CNResult,   Rule#verify.expected_val),
  % io:format("\e[95m[dbg]\e[0m check_expected [~p] \e[35m~p\e[0m\n", [?LINE, CEResult]),
  VTResult = validate_type(Key,        CEResult,  Rule#verify.expected_type),
  % io:format("\e[95m[dbg]\e[0m validate_type [~p] \e[35m~p\e[0m\n", [?LINE, VTResult]),
  VRResult = validate_range(Key,       VTResult, Rule#verify.expected_range),
  % io:format("\e[95m[dbg]\e[0m validate_range [~p] \e[35m~p\e[0m\n", [?LINE, VRResult]),
  VRgxpRes = validate_by_reg_exp(Key,  VRResult,   Rule#verify.regexp_check),
  % io:format("\e[95m[dbg]\e[0m validate_by_reg_exp [~p] \e[35m~p\e[0m\n", [?LINE, VRgxpRes]),
  case VRgxpRes of
    {true, _, NewValue} ->
      setd('$validation_errors', [
        {key, getd('$$c_key', Key)},
        {res, [CNResult, CEResult, VTResult, VRResult, VRgxpRes]}]),
      {true, replace_val(Key, NewValue, Object, Rule#verify.replace_val)};
    {false , _} = Fail ->
      setd('$validation_errors', [
        {key, getd('$$c_key', Key)},
        {res, [CNResult, CEResult, VTResult, VRResult, VRgxpRes]}]),
      Fail
  end.

check_or([Rule|Rest], Object, Acc) ->
  case check(Rule, Object) of
    {true, NewObj} ->
      check_or(Rest, NewObj, [true|Acc]);
    {false, Err} ->
      setd('$_or_err', Err),
      check_or(Rest, Object, [false|Acc])
  end;
check_or([], Object, Acc) ->
  case length([Bool||Bool<-Acc, Bool =/= false]) of
    0 -> {false, getd('$_or_err')};
    _ -> {true,  Object}
  end.

check_and([Rule|Rest], Object, true) ->
  {NewACC, Result} = check(Rule, Object),
  check_and(Rest, Result, NewACC == true);

check_and(_, Object, Acc) ->
  {Acc, Object}.


check_necessary(Key, {true, _, '$not_found!'}, true) ->
  {false, [{error, Key}, {necessary_param, miss}]};
check_necessary(_Key, {true, _, Value}, true) ->
  {true,   necessary, Value};
check_necessary(_Key, {true, _, Value}, _) ->
  {true, unnecessary, Value}.

check_expected(Key, {true, _, Value}, ExpVals) when is_list(ExpVals)  ->
  case lists:member(Value, ExpVals) of
    true ->
      {true, expected, Value};
    _ ->
      {false, [{error, Key}, {expected_val, ExpVals}]}
  end;
check_expected(_, {true, _, Value}, any) ->
  {true, expected, Value};
check_expected(Key, {true, _, Value}, ExpValue) ->
  case Value =:= ExpValue of
    true -> {true, expected, Value};
    _ ->
      {false, [{error, Key}, {expected_val, ExpValue}]}
  end;
check_expected(_, Fail, _) ->
  Fail.

validate_type(Key, {true, _, _} = Value, any) ->
  validate_type(Key, Value, [map, list, atom, binary, bin_int, bin_float, float, integer]);
validate_type(Key, {true, _, _} = Value, Type) when is_list(Type) ->
  validate_type_loop(Key, Value, Type);

validate_type(Key, {true, _, Value}, Type) ->
  if
    is_map(Value),     Type == map     -> {true, map,     Value};
    is_list(Value),    Type == list    -> checklist(Key, Value, Type);
    is_list(Value),    Type == map     -> cast_list_to_map(Key, Value, Type);
    is_atom(Value),    Type == atom    -> {true, atom,    Value};
    is_float(Value),   Type == float   -> {true, float,   Value};
    is_binary(Value),  Type == binary  -> {true, binary,  Value};
    is_integer(Value), Type == integer -> {true, integer, Value};
    is_binary(Value),  (Type == integer orelse Type == bin_int) -> cast_bin_to_int(Key, Value, Type);
    is_binary(Value),  (Type == float orelse Type == bin_float) -> cast_bin_to_float(Key, Value, Type);
    true ->  {false, [{error, Key}, {expected_type, Type}]}
  end;
validate_type(_, Fail, _) ->
  Fail.

validate_type_loop(Key, Value, Type) ->
  case validate_type_loop(Key, Value, Type, [], []) of
    {[],Invalid} ->
      zip_errors(Key, Invalid, expected_type);
    {Valid, _} ->
      erlang:hd(Valid)
  end.
validate_type_loop(Key, Value, [Type|Types], Valid, Invalid) ->
  case validate_type(Key, Value, Type) of
    {true, _, _} = Result ->
      validate_type_loop(Key, Value, Types, [Result|Valid], Invalid);
    False ->
      validate_type_loop(Key, Value, Types, Valid, [False|Invalid])
  end;
validate_type_loop(_, _, [], Valid, Invalid) ->
  {Valid, Invalid}.

checklist(Key, Value, Type) ->
  case cast_list_to_map(Key, Value, Type) of
    {true, _, _} -> {false, [{error, Key}, {expected_type, Type}]};
    _ -> {true, list, Value}
  end.

cast_list_to_map(Key, Value, Type) ->
  case catch maps:from_list(Value) of
    Result when is_map(Value) -> {true, map, Result};
    _ -> {false, [{error, Key}, {expected_type, Type}]}
  end.

cast_bin_to_int(Key, Value, Type) ->
  try binary_to_integer(Value) of
    Result -> {true, integer, Result}
  catch
    _ : _  -> {false, [{error, Key}, {expected_type, Type}]}
  end.

cast_bin_to_float(Key, Value, Type) ->
  try binary_to_float(Value) of
    Result -> {true, float,  Result}
  catch
    _ : _  -> {false, [{error, Key}, {expected_type, Type}]}
  end.


validate_range(Key, {true, Type, Value}, {max, N}=Rule) ->
  Result =  if
              Type == list   -> erlang:length(Value) =< N;
              Type == map    -> erlang:length(maps:to_list(Value)) =< N;
              (Type == float) or (Type == integer) -> Value =< N;
              Type == binary -> erlang:size(Value) =< N;
              true -> true
            end,
  case Result of
    true ->
      {true, Type, Value};
    _ ->
      {false, [{error, Key}, {exp_range, [Rule]}]}
  end;
validate_range(Key, {true, Type, Value}, {min, N})  ->
  Result =  if
              Type == list   -> ((erlang:length(Value) >= N) orelse (N == infinity));
              Type == map    -> erlang:length(maps:to_list(Value)) >= N;
              (Type == float) or (Type == integer) -> Value >= N;
              Type == binary -> erlang:size(Value) >= N;
              true -> true
            end,
  case Result of
    true ->
      {true, Type, Value};
    _ ->
      {false, [{error, Key}, {exp_range, [{min, N}]}]}
  end;
validate_range(Key, {true, Type, Value}, N) when is_integer(N) ->
  Result =  if
              Type == list   -> erlang:length(Value) == N;
              Type == map    -> erlang:length(maps:to_list(Value)) == N;
              (Type == float) or (Type == integer) -> Value == N;
              Type == binary -> erlang:size(Value) == N;
              true -> true
            end,
  case Result of
    true ->
      {true, Type, Value};
    _ ->
      {false, [{error, Key}, {exp_range, N}]}
  end;
validate_range(Key, {true, Type, Value}, {Min, Max}) ->
  Result =  if
              Type == list   -> (erlang:length(Value) >= Min) andalso (erlang:length(Value) =< Max);
              Type == map    -> S = erlang:length(maps:to_list(Value)), ((S >= Min) andalso (S =< Max));
              (Type == float) or (Type == integer) -> ((Value >= Min) and (Value =< Max));
              Type == binary -> S = erlang:size(Value), ((S >= Min) and (S =< Max));
              true -> true
            end,
  case Result of
    true ->
      {true, Type, Value};
    _ ->
      {false, [{error, Key}, {exp_range, [{max, Max},{min, Min}]}]}
  end;
validate_range(_Key, {true, Type, Obj}, _Rule) -> %% for 'any' rule
  {true, Type, Obj};
validate_range(_Key, Fail, _Rule) ->
  Fail.


validate_by_reg_exp(_, {true, _, Value}, false) ->
  {true, regexp, Value};
validate_by_reg_exp(Key, {true, Type, Value}, Rules) when ((Type == list) or (Type == binary)) ->
  case validate_by_reg_exp_loop(Key, Value, Rules, [], []) of
    {[],      []} -> {true, regexp, Value};
    {[], Invalid} -> zip_errors(Key, Invalid, regexp);
    {Valid,    _} -> erlang:hd(Valid)
  end;
validate_by_reg_exp(_, Fail, _) ->
  Fail.

validate_by_reg_exp_loop(Key, Value, [REGEXP|RestExpressions], AccOk, AccERR) ->
  case re:run(Value, REGEXP) of
    match      -> validate_by_reg_exp_loop(Key, Value, RestExpressions, [{true, REGEXP, Value}|AccOk], AccERR);
    {match, _} -> validate_by_reg_exp_loop(Key, Value, RestExpressions, [{true, REGEXP, Value}|AccOk], AccERR);
    nomatch    -> validate_by_reg_exp_loop(Key, Value, RestExpressions, AccOk, [{false, [{error, Key}, {regexp, [{nomatch, REGEXP}]}]}|AccERR]);
    Error      -> Error, validate_by_reg_exp_loop(Key,       Value, RestExpressions, AccOk, [{false, [{error, Key}, {regexp, REGEXP}]}|AccERR])
  end;
validate_by_reg_exp_loop(_Key, _Value, [], AccOk, AccERR) ->
  {AccOk, AccERR}.





replace_val(Key, Value, Object, true) ->
  set(Key, Value, Object, true);
replace_val(_, _, Object, _) ->
  Object.

%% UTILITES BLOCK
zip_errors(Key, Invalid, KeyE) ->
  {false, [{error, Key}, {KeyE, zip2_errors(Invalid, KeyE)}]}.

zip2_errors([{false, List}|Invalid], Key) ->
  [get(Key, List)|zip2_errors(Key, Invalid)];
zip2_errors(_, _) ->
  [].

get(Key, Struct) ->
  get(Key, Struct, undefined).

get(Key, Struct, Default) when is_list(Struct) ->
  case lists:keyfind(Key, 1, Struct) of
    false      -> Default;
    {_, Value} -> Value
  end;

get(Key, Struct, Default) when is_map(Struct) ->
  maps:get(Key, Struct, Default).

set(Key, Value, Struct) ->
  set(Key, Value, Struct, true).

set(Key, Value, Struct, Override) when is_list(Struct) ->
  case Override of
    true -> lists:keystore(Key,   1, Struct, {Key, Value});
    _    -> lists:keyreplace(Key, 1, Struct, {Key, Value})
  end;
set(Key, Value, Struct, Override) when is_map(Struct) ->
  case Override of
    true -> maps:put(Key, Value, Struct);
    _    -> update(Key, Value, Struct)
  end.

update(Key, Value, Map) ->
  case catch maps:update(Key, Value, Map) of
    Map1 when is_map(Map1) -> Map1;
    Map2                   -> Map2
  end.


getd(Key) ->
  getd(Key, undefined).
getd(Key, Default) ->
  case erlang:get(Key) of
    undefined -> Default;
    Rest      -> Rest
  end.

setd(Key, Val) ->
  setd(Key, Val, true).
-spec setd (Key :: any(), Value :: any(), Is_append_need :: boolean()) -> _.
setd(Key, Val, true) ->
  setd(Key, [Val|getd(Key, [])], false);
setd(Key, Value, _) ->
  erlang:put(Key, Value).
