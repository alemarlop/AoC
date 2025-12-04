defmodule Day19 do
  :ets.new(:patterns_cache, [:named_table, :public, read_concurrency: true])
  :ets.new(:performed_ops, [:named_table, :public, read_concurrency: true])

  defp get_cache(pattern) do
    case :ets.lookup(:patterns_cache, pattern) do
      [{^pattern, _}] -> true
      [] -> :false
    end
  end

  defp put_cache(pattern, value), do: :ets.insert(:patterns_cache, {pattern, value})

  defp put_op(op) do
    case :ets.lookup(:performed_ops, :list) do
      [{:list, ops}] -> :ets.insert(:performed_ops, {:list, [op | ops]})
      [] -> :ets.insert(:performed_ops, {:list, [op]})
    end
  end

  def find_coincidences_pattern(_, ""), do: true
  def find_coincidences_pattern(pieces, pattern) do
    case get_cache(pattern) do
      true -> IO.inspect("hit!") && false
      false -> find_coincidences_pattern(pieces, pattern, :no_cache)
    end
  end

  def find_coincidences_pattern(pieces, pattern, :no_cache) do
    put_op(pattern)
    case Enum.filter(pieces, fn piece -> String.starts_with?(pattern, piece) end) do
      [] -> put_cache(pattern, true) && false
      coincidences ->
        Enum.map(coincidences, fn coincidence ->
          n_pattern = String.replace(pattern, coincidence, "")
          n_pieces = List.delete(pieces, coincidence)
          find_coincidences_pattern(n_pieces, n_pattern)
        end) |> Enum.any?(fn x -> x end)
    end
  end

  def count_patterns(_, []), do: 0
  def count_patterns(pieces, [h | t]) do
    IO.inspect("Pattern: #{h}")
    (find_coincidences_pattern(pieces, h) && 1 || 0) + count_patterns(pieces, t)
  end
end

[pieces, patterns] = hd(System.argv)
|> File.read!()
|> String.split("\n\n", trim: true)

pieces = String.split(pieces, ", ", trim: true)
patterns = String.split(patterns, "\n", trim: true)

Day19.count_patterns(pieces, patterns) |> IO.inspect()
IO.inspect(:ets.tab2list(:performed_ops) |> hd() |> elem(1) |> Enum.frequencies())
