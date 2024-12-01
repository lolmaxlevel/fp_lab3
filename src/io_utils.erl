-module(io_utils).
-export([read_input/2, write_points/2]).

% Function to read input
read_input(MainPid, FileIo) ->
  case FileIo of
    true -> WelcomeStr = "";
    _ -> WelcomeStr = "Enter Points in format X Y> "
  end,
  case io:fread(WelcomeStr, "~f ~f") of
    {ok, [X, Y]} ->
      MainPid ! {self(), {X, Y}},
      timer:sleep(100),
      read_input(MainPid, FileIo);
    eof ->
      MainPid ! done
  end.

write_points(Data, Method) ->
  % Extract X and Y values
  Xs = [X || {X, _} <- Data],
  Ys = [Y || {_, Y} <- Data],
  io:format("Interpolated points using ~s method:~n", [Method]),
  lists:foreach(fun(X) -> io:format("~.2f\t", [X]) end, Xs),
  io:format("~n"),

  lists:foreach(fun(Y) -> io:format("~.2f\t", [Y]) end, Ys),
  io:format("~n").
