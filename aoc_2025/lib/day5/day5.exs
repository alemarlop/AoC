import Utils

defmodule Day5 do
  def run(func) do
    {ranges, ids} = format_inupt()

    ranges
    |> MapSet.new()
    |> then(&deep_merge(&1, merge_ranges(&1)))
    |> then(&func.(&1, ids))
  end

  def ids_in_range(ranges, ids) do
    ids
    |> Enum.filter(fn id -> Enum.any?(ranges, fn {from, to} -> id >= from and id <= to end) end)
    |> Enum.count()
  end

  def count_ranges(ranges, _ids) do
    ranges
    |> Enum.map(fn {from, to} -> to - from + 1 end)
    |> Enum.sum()
  end

  defp deep_merge(ranges, ranges), do: ranges
  defp deep_merge(_ranges, new_ranges), do: deep_merge(new_ranges, merge_ranges(new_ranges))

  defp merge_ranges(ranges) do
    Enum.reduce(ranges, MapSet.new([]), fn {from, to}, acc ->
      other_ranges = MapSet.delete(ranges, {from, to})

      case Enum.find(other_ranges, fn {fi, ti} -> not Range.disjoint?(from..to, fi..ti) end) do
        nil -> MapSet.put(acc, {from, to})
        {fi, ti} -> MapSet.put(acc, {min(from, fi), max(to, ti)})
      end
    end)
  end

  defp format_inupt() do
    [raw_ranges, raw_ids] =
      read_from_args(fn content -> String.split(content, "\n\n", trim: true) end)

    ranges =
      raw_ranges
      |> String.split("\n", trim: true)
      |> Enum.map(fn line -> Regex.scan(~r/(\d+)\-(\d+)/, line) |> regex_result_to_range() end)

    {ranges, Enum.map(String.split(raw_ids, "\n"), &String.to_integer/1)}
  end

  defp regex_result_to_range([[_range, from, to]]),
    do: {String.to_integer(from), String.to_integer(to)}
end

run_t(fn -> Day5.run(&Day5.ids_in_range/2) end)
run_t(fn -> Day5.run(&Day5.count_ranges/2) end)
