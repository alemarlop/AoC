defmodule Utils do
  @spec read_from_args((binary -> term)) :: [term]
  def read_from_args(parser \\ fn t -> t end) do
    System.argv()
    |> hd()
    |> File.read!()
    |> parser.()
  end

  @spec lines_from_args((binary -> term)) :: [term]
  def lines_from_args(parser \\ fn t -> t end) do
    System.argv()
    |> hd()
    |> File.read!()
    |> lines(parser)
  end

  @spec lines_from_file(binary, (binary -> term)) :: [term]
  def lines_from_file(file_name, parser \\ fn t -> t end) do
    file_name
    |> File.read!()
    |> lines(parser)
  end

  @spec lines(binary, (binary -> term)) :: [term]
  def lines(raw_input, parser \\ fn t -> t end) do
    raw_input
    |> String.split("\n", trim: true)
    |> Enum.map(&parser.(&1))
  end

  @spec run_t((-> any), binary) :: :ok
  def run_t(func, title \\ "- ") do
    start_time = System.monotonic_time(:nanosecond)
    result = func.()
    end_time = System.monotonic_time(:nanosecond)
    time = end_time - start_time

    {unit, time} =
      cond do
        time < 1_000 -> {" ns", time}
        time < 1_000 * 1_000 -> {" μs", time / 1_000}
        time < 1_000 * 1_000 * 1_000 -> {" ms", time / (1_000 * 1_000)}
        true -> {" s", time / (1_000 * 1_000 * 1_000)}
      end

    IO.puts("[#{inspect(time) <> unit}] " <> title <> inspect(result))
  end
end
