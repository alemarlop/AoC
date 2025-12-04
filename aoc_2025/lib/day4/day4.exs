import Utils

defmodule Day4 do
  @adjacent_offsets [{0, 1}, {1, 0}, {0, -1}, {-1, 0}, {1, 1}, {-1, 1}, {1, -1}, {-1, -1}]

  def run(func) do
    lines_from_args(fn line -> String.split(line, "", trim: true) end)
    |> to_mapset()
    |> then(&func.(&1))
  end

  def get_accessible_rolls(rolls_set) do
    accessible_rolls =
      Enum.filter(rolls_set, fn {x, y} ->
        @adjacent_offsets
        |> Enum.filter(fn {xi, yi} -> MapSet.member?(rolls_set, {x + xi, y + yi}) end)
        |> Enum.count() < 4
      end)

    MapSet.new(accessible_rolls)
  end

  def count_all_accesible(rolls_set, acc) do
    accessible_rolls = get_accessible_rolls(rolls_set)
    next_rolls_set = MapSet.difference(rolls_set, accessible_rolls)

    if rolls_set == next_rolls_set,
      do: acc,
      else: count_all_accesible(next_rolls_set, MapSet.size(accessible_rolls) + acc)
  end

  defp to_mapset(grid) do
    positions =
      for x <- 0..(length(hd(grid)) - 1),
          y <- 0..(length(grid) - 1),
          Enum.at(Enum.at(grid, y), x) == "@",
          do: {x, y}

    MapSet.new(positions)
  end
end

run_t(fn -> Day4.run(&Day4.get_accessible_rolls/1) |> MapSet.size() end)
run_t(fn -> Day4.run(fn rolls -> Day4.count_all_accesible(rolls, 0) end) end)
