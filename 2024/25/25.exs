defmodule Day25 do
  def run(file_name) do
    File.read!(file_name)
    |> format_input()
    |> classify_and_parse()
    |> count_matches()
  end

  defp format_input(raw_input) do
    String.split(raw_input, "\n\n", trim: true)
    |> Enum.map(fn square ->
      String.split(square, "\n", trim: true)
      |> Enum.map(fn line -> String.split(line, "", trim: true) end)
    end)
  end

  defp classify_and_parse(squares) do
    Enum.reduce(squares, {[], []}, fn square, {locks, keys} ->
      values = Enum.map(rotate_90_clockwise(square), fn row -> Enum.count(row, & &1 == "#") - 1 end)
      case lock_or_key?(square) do
        :lock -> {[values | locks], keys}
        :key -> {locks, [values | keys]}
      end
    end)
  end

  defp count_matches({locks, keys}),
    do: Enum.reduce(locks, 0, fn lock, acc -> acc + count_matches(lock, keys) end)

  defp count_matches(_, []), do: 0
  defp count_matches(lock, [fst_k | t]) do
    val = Enum.all?(Enum.zip(lock, fst_k), fn {l, k} -> l + k <= 5 end) && 1 || 0
    val + count_matches(lock, t)
  end

  defp lock_or_key?([fst | _]), do: Enum.all?(fst, & &1 == "#") && :lock || :key

  defp rotate_90_clockwise(matrix) do
    matrix
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.map(&Enum.reverse/1)
  end
end

Day25.run(hd(System.argv())) |> IO.inspect()
