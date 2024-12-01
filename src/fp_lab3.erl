-module(fp_lab3).
-export([main/1]).

%% internal functions
main(Args) ->
    argparse:run(Args, cli(), #{progname => fp_lab3}).

cli() ->
    #{
        arguments => [
            #{name => method, short => $m, type => string, default => "all"},
            #{name => frequency, short => $f, type => float, default => 1.0},
            #{name => fileio, short => $i, type => boolean, default => false}
        ],
        handler =>
            fun (#{method := Method, frequency := Frequency, fileio := FileIo}) ->
                MainPid = self(),
                IoPid = spawn(io_utils, read_input, [MainPid, FileIo]),
                receive_input(IoPid, Method, Frequency, [])
            end
    }.

receive_input(IoPid, Method, Frequency, InputData) ->
    receive
        {IoPid, {X, Y}} ->
            NewInputData = [{X, Y} | InputData],
            interpolate(self(), Method, Frequency, NewInputData),
            receive_input(IoPid, Method, Frequency, NewInputData);
        done ->
            io:format("Input reading done~n"),
            ok
    end.

interpolate(MainPid, Method, Freq, Data) ->
    IntPid = spawn(interpolation, interpolate, [MainPid, Method, Freq, lists:reverse(Data)]),
    receive
        {IntPid, LinearData, NewtonData} ->
            case LinearData of
                ok -> pass;
                _ -> io_utils:write_points(LinearData, "linear")
            end,
            case NewtonData of
                ok -> pass;
                _ -> io_utils:write_points(NewtonData, "newton")
            end
    end.