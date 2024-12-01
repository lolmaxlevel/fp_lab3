# Лабораторная работа №3 по дисциплине "Функциональное Программирование"

## Терновский Илья Евгеньевич, P3332

## Вариант: `Интерполяция методом Ньютона`

## Задание

В рамках лабораторной работы вам предлагается повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции (в разные годы это лабораторная работа 3 или 4) со следующими дополнениями:

## Требования:

- [X] обязательно должна быть реализована линейная интерполяция (отрезками, link); 
- [X] настройки алгоритма интерполяции и выводимых данных должны задаваться через аргументы командной строки:
   - [X] какие алгоритмы использовать (в том числе два сразу);
   - [X] частота дискретизации результирующих данных;
- [X] входные данные должны задаваться в текстовом формате на подобии ".csv" (к примеру x;y\n или x\ty\n) и подаваться на стандартный ввод, входные данные должны быть отсортированы по возрастанию x;
- [X] выходные данные должны подаваться на стандартный вывод;
- [X] программа должна работать в потоковом режиме (пример -- cat | grep 11), это значит, что при запуске программы она должна ожидать получения данных на стандартный ввод, и, по мере получения достаточного количества данных, должна выводить рассчитанные точки в стандартный вывод;

## Немного о реализации
В данной лабораторной работе я использовал стандартные потоки, то есть не использовал модели поведения(их решил оставить на последнюю).
Основной модуль [fp_lab3.erl](src/fp_lab3.erl)
Основной модуль является boilerplate файлом, он получает аргументы командной строки, создает отдельный поток для чтения
чисел из стандартного ввода и записи туда же. После получения новой точки выполняется интерполяция методами указаными в 
аргументах.

Запуск программы и получение аргументов.
```erlang
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
```
Функции для чтения и записи данных и интерполяции:
```erlang
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
```

Модуль интерполяции [interpolation.erl](src/interpolation.erl)
В данном модуле выполняются все операции по интерполяции, так же как и модуль для работы с IO является модулем, который
следует использовать в отдельном потоке. Так же в этом модуле реализуется и генерация точек для интерполяции.

Основная функция интерполяции:
```erlang
interpolate(MainPid, Method, Frequency, Data) ->
  % Perform interpolation
  case Method of
    "linear" -> LinearData = case length(Data) >= 2 of
                 true -> linear_interpolation(Frequency, Data);
                 false -> ok
               end,
                 NewtonData = ok;
    "newton" -> NewtonData = case length(Data) >= 4 of
                 true -> newton_interpolation(Frequency, Data);
                 false -> ok
               end,
                 LinearData = ok;
    "all" -> LinearData = case length(Data) >= 2 of
               true -> linear_interpolation(Frequency, Data);
               false -> ok
             end,
               NewtonData = case length(Data) >= 4 of
               true -> newton_interpolation(Frequency, Data);
               false -> ok
             end;
    _ -> io:format("Unknown interpolation method: ~s~n", [Method]),
          LinearData = ok,
          NewtonData = ok
  end,
  MainPid ! {self(), LinearData, NewtonData}.
```

Функция линейной интерполяции:
```erlang
linear_interpolation(Frequency, Data) ->
  [Last1, Last2 | _] = lists:reverse(Data),
  linear_interpolation(Frequency, [Last2, Last1], []).

linear_interpolation(Frequency, [{X1, Y1}, {X2, Y2}], Acc) when X1 =< X2 ->
  InterpolatedPoints = generate_seq(X1, X2 + Frequency, Frequency),
  InterpolatedValues = [{X, Y1 + (Y2 - Y1) * (X - X1) / (X2 - X1)} || X <- InterpolatedPoints],
  Acc ++ InterpolatedValues;
linear_interpolation(_, _, Acc) ->
  Acc.
```

Функция интерполяции методом Ньютона:
```erlang
newton_interpolation(Frequency, Data) ->
  [Last1, Last2, Last3, Last4 | _] = lists:reverse(Data),
  PointsToInterpolate = generate_seq(element(1, Last4), element(1, Last1) + Frequency, Frequency),
  InterpolatedValues = [{X,
    newton_polynomial([Last4, Last3, Last2, Last1], X)} || X <- PointsToInterpolate],
  InterpolatedValues.

newton_polynomial(Data, X) ->
  [{X1, Y1}, {X2, Y2}, {X3, Y3}, {X4, Y4}] = Data,
  DivDiff1 = (Y2 - Y1) / (X2 - X1),
  DivDiff2 = ((Y3 - Y2) / (X3 - X2) - DivDiff1) / (X3 - X1),
  DivDiff3 = (((Y4 - Y3) / (X4 - X3) - (Y3 - Y2) / (X3 - X2))
    / (X4 - X2) - DivDiff2) / (X4 - X1),
  Y1 + (X - X1) * DivDiff1 + (X - X1) * (X - X2) * DivDiff2 +
    (X - X1) * (X - X2) * (X - X3) * DivDiff3.
```

Функция генерации последовательности:
```erlang
generate_seq(Start, End, Step) when Start =< End ->
  generate_seq(Start, End, Step, []);
generate_seq(Start, End, Step) when Start > End ->
  generate_seq(End, Start, Step, []).

generate_seq(Current, End, Step, Acc) when Current =< End ->
  generate_seq(Current + Step, End, Step, [Current | Acc]);
generate_seq(_, _, _, Acc) ->
  lists:reverse(Acc).
```

Модуль для работы с IO [io_utils.erl](src/io_utils.erl)
Как и было описано выше, в данном модуле производится работа со стандартным IO. Как можно заметить числа читаются только в формате с плавающей точкой.
```erlang
read_input(MainPid) ->
  case io:fread("Enter Points in format X Y> ", "~f ~f") of
    {ok, [X, Y]} ->
      MainPid ! {self(), {X, Y}},
      timer:sleep(100),
      read_input(MainPid);
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
```
Пример работы программы при вводе данных из файла:
```
PS D:\projects\fp_lab3> cat .\input.txt | .\_build\default\bin\fp_lab3 all 1.0
Interpolated points using linear method:
2.10    3.10    4.10
3.30    1.68    0.07
Interpolated points using linear method:
3.40    4.40    5.40
1.20    81.93   162.65
Interpolated points using linear method:
4.50    5.50
90.00   30.56
Interpolated points using newton method:
2.10    3.10    4.10    5.10    6.10
3.30    -22.60  66.75   71.52   -208.13
Interpolated points using linear method:
5.32    6.32    7.32
41.20   73.64   106.07
Interpolated points using newton method:
3.40    4.40    5.40    6.40    7.40
1.20    92.32   36.10   34.51   289.50
Interpolated points using linear method:
6.77    7.77
88.20   90.93
Interpolated points using newton method:
4.50    5.50    6.50    7.50
90.00   42.80   80.56   71.32
Input reading done
```
Пример работы программы при вводе данных вручную:
```
PS D:\projects\fp_lab3> .\_build\default\bin\fp_lab3
Enter Points in format X Y> 0.0 0.0
Enter Points in format X Y> 1.571 1.0
Interpolated points using linear method:
0.00    1.00    2.00
0.00    0.64    1.27
Enter Points in format X Y> 3.142 0.0
Interpolated points using linear method:
1.57    2.57    3.57
1.00    0.36    -0.27
Enter Points in format X Y> 4.712 -1.0
Interpolated points using linear method:
3.14    4.14    5.14
0.00    -0.64   -1.27
Interpolated points using newton method:
0.00    1.00    2.00    3.00    4.00    5.00
0.00    0.97    0.84    0.12    -0.67   -1.03
Enter Points in format X Y> 12.568 0.0
Interpolated points using linear method:
4.71    5.71    6.71    7.71    8.71    9.71    10.71   11.71   12.71
-1.00   -0.87   -0.75   -0.62   -0.49   -0.36   -0.24   -0.11   0.02
Interpolated points using newton method:
1.57    2.57    3.57    4.57    5.57    6.57    7.57    8.57    9.57    10.57   11.57   12.57
1.00    0.37    -0.28   -0.91   -1.49   -1.95   -2.26   -2.38   -2.25   -1.84   -1.11   0.00
```

## Особенности работы
Программа принимает на вход только числа с плавающей точкой, сделано так, потому что обрабатывать ввод с любым форматом крайне неприятное действие.
Программа имеет возможность работать в потоковом режиме, что позволяет обрабатывать большие объемы данных.
Программа имеет возможность выбора метода интерполяции, а так же частоты дискретизации.
Входные данные должны быть в формате X Y\n, где X и Y числа с плавающей точкой.

# Вывод
Во время выполнения работы я познакомился с новыми возможностями языка Erlang, такими как работа с потоками и аргументами командной строки.
Так же понял почему эрланг не особо пользуется популярностью для создания cli приложений =)
Познакомился с потоками на очень базовом уровне.
