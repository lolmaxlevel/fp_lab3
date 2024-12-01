-module(interpolation_unit_SUITE).

-include_lib("../src/interpolation.erl").
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([generate_seq_test/1, linear_interpolation_test/1, newton_interpolation_test/1, interpolate_test/1]).

all() ->
  [generate_seq_test, linear_interpolation_test, interpolate_test, newton_interpolation_test].


% Test for generate_seq/3
generate_seq_test(_) ->
  ?assertEqual([0], generate_seq(0, 0, 1)),
  ?assertEqual([0, 1], generate_seq(0, 1, 1)),
  ?assertEqual([0, 0.4, 0.8, 1.2], generate_seq(0, 1.2, 0.4)),
  ?assertEqual([-1, -0.5, 0], generate_seq(-1, 0, 0.5)).

% Test for linear_interpolation/2
linear_interpolation_test(_) ->
  Data = [{0, 0}, {1, 1}],
  ?assertEqual([{0, 0.0}, {1, 1.0}], linear_interpolation(1, Data)),
  Data2 = [{0, 0}, {2, 4}],
  ?assertEqual([{0, 0.0}, {1, 2.0}, {2, 4.0}], linear_interpolation(1, Data2)),
  Data3 = [{0, 0}, {5, 10}],
  ?assertEqual([{0, 0.0}, {2.5, 5.0}, {5, 10.0}], linear_interpolation(2.5, Data3)).

% Test for newton_interpolation/2
newton_interpolation_test(_) ->
  Data = [{0, 0}, {1, 1}, {2, 4}, {3, 9}],
  Frequency = 0.5,
  Result = newton_interpolation(Frequency, Data),
  Expected = [{0.0, 0.0}, {0.5, 0.25}, {1.0, 1.0}, {1.5, 2.25}, {2.0, 4.0}, {2.5, 6.25}, {3.0, 9.0}],
  ?assertEqual(Expected, Result).

% Test for interpolate/3
interpolate_test(_) ->
  MainPid = self(),
  Frequency = 1,
  DataLinear = [{0, 0}, {2, 4}],
  interpolate(MainPid, Frequency, DataLinear),
  receive
    {_, LinearResult, ok} ->
      ?assertEqual([{0, 0.0}, {1, 2.0}, {2, 4.0}], LinearResult)
  end,
  DataNewton = [{0, 0}, {1, 1}, {2, 4}, {3, 9}],
  interpolate(MainPid, Frequency, DataNewton),
  receive
    {_, LinearResult, NewtonResult} ->
      ?assertEqual([{0, 0.0}, {1, 2.0}, {2, 4.0}], LinearResult),
      ?assertEqual([{0.0, 0.0}, {1.0, 1.0}, {2.0, 4.0}, {3.0, 9.0}], NewtonResult)
  end.