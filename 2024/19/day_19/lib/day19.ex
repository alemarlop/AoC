defmodule Day19 do
  use Memoize

  defmemo find_coincidences_pattern(pieces, pattern) do
    case Enum.filter(pieces, fn piece -> String.starts_with?(pattern, piece) end) do
      [] -> false
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
