-module(interpolation).
-export([linear_interpolation/2, newton_interpolation/2, interpolate/4, generate_seq/3]).

interpolate(MainPid, Method, Frequency, Data) ->
  % Perform interpolation
  {LinearData, NewtonData} = perform_interpolation(Method, Frequency, Data),
  MainPid ! {self(), LinearData, NewtonData}.

perform_interpolation("linear", Frequency, Data) ->
  {linear_interpolation(Frequency, Data), ok};
perform_interpolation("newton", Frequency, Data) ->
  {ok, newton_interpolation(Frequency, Data)};
perform_interpolation("all", Frequency, Data) ->
  {linear_interpolation(Frequency, Data), newton_interpolation(Frequency, Data)};
perform_interpolation(_, _, _) ->
  io:format("Unknown interpolation method~n"),
  {ok, ok}.

linear_interpolation(_, Data) when length(Data) < 2 ->
  ok;
% Linear interpolation
linear_interpolation(Frequency, Data) when length(Data) >= 2 ->
  [Last1, Last2 | _] = lists:reverse(Data),
  linear_interpolation(Frequency, [Last2, Last1], []);
linear_interpolation(_, _) ->
  [].

linear_interpolation(Frequency, [{X1, Y1}, {X2, Y2}], Acc) when X1 =< X2 ->
  InterpolatedPoints = generate_seq(X1, X2 + Frequency, Frequency),
  InterpolatedValues = [{X, Y1 + (Y2 - Y1) * (X - X1) / (X2 - X1)} || X <- InterpolatedPoints],
  Acc ++ InterpolatedValues;
linear_interpolation(_, _, Acc) ->
  Acc.

% Custom function to generate a sequence of floating-point numbers
generate_seq(Start, End, Step) when Start =< End ->
  generate_seq(Start, End, Step, []);
generate_seq(Start, End, Step) when Start > End ->
  generate_seq(End, Start, Step, []).

generate_seq(Current, End, Step, Acc) when Current =< End ->
  generate_seq(Current + Step, End, Step, [Current | Acc]);
generate_seq(_, _, _, Acc) ->
  lists:reverse(Acc).

newton_interpolation(_, Data) when length(Data) < 4 ->
  ok;
newton_interpolation(Frequency, Data) when length(Data) >= 4 ->
  [Last1, Last2, Last3, Last4 | _] = lists:reverse(Data),
  PointsToInterpolate = generate_seq(element(1, Last4), element(1, Last1) + Frequency, Frequency),
  InterpolatedValues = [{X, newton_polynomial([Last4, Last3, Last2, Last1], X)}
    || X <- PointsToInterpolate],
  InterpolatedValues.

newton_polynomial(Data, X) ->
  [{X1, Y1}, {X2, Y2}, {X3, Y3}, {X4, Y4}] = Data,
  DivDiff1 = (Y2 - Y1) / (X2 - X1),
  DivDiff2 = ((Y3 - Y2) / (X3 - X2) - DivDiff1) / (X3 - X1),
  DivDiff3 = (((Y4 - Y3) / (X4 - X3) - (Y3 - Y2) / (X3 - X2))
    / (X4 - X2) - DivDiff2) / (X4 - X1),
  Y1 + (X - X1) * DivDiff1 + (X - X1) * (X - X2)
    * DivDiff2 + (X - X1) * (X - X2) * (X - X3) * DivDiff3.