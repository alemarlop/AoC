import Utils

defmodule Day2 do
  def run(min_counter) do
    read_from_args(fn raw_input -> String.split(raw_input, ",", trim: true) end)
    |> Enum.map(&format_range/1)
    |> Enum.map(&n_in_range(&1, min_counter))
    |> Enum.sum()
  end

  defp format_range(input) do
    [from, to] = String.split(input, "-", trim: true)
    String.to_integer(from)..String.to_integer(to)
  end

  defp n_in_range(range, min_counter) do
    Enum.reduce(range, 0, fn elem, acc ->
      num_as_list = String.split(to_string(elem), "", trim: true)
      counter = if min_counter == :half, do: max(div(length(num_as_list), 2), 1), else: 1
      acc + check_num(counter, num_as_list)
    end)
  end

  def check_num(counter, list) when counter > div(length(list), 2) + 1, do: 0

  def check_num(counter, list) do
    list
    |> Enum.chunk_every(counter)
    |> Enum.frequencies()
    |> then(&(length(Map.keys(&1)) == 1 && hd(Map.values(&1)) > 1))
    |> then(&((&1 && String.to_integer(Enum.join(list))) || check_num(counter + 1, list)))
  end
end

run_t(fn -> Day2.run(:half) end)
run_t(fn -> Day2.run(:any) end)
