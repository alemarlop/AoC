defmodule Day8 do
  def c_points({r1,c1}, {r2, c2}, l) do
    {dr, dc} = {r1-r2, c1-c2}
    [in_bounds({r1+dr, c1+dc}, l) || {}, in_bounds({r2-dr, c2-dc}, l) || {}]
  end

  def c_points_2({r1,c1}, {r2, c2}, l) do
    offset = {r1-r2, c1-c2}
    [{r1,c1}, c_points_2({r1,c1}, offset, l, &+/2), {r2, c2}, c_points_2({r1,c1}, offset, l, &-/2)] |> List.flatten()
  end
  def c_points_2({r1,c1}, {dr, dc}, l, fun) do
    case in_bounds({fun.(r1,dr), fun.(c1,dc)}, l) do
      false -> []
      point -> [point | c_points_2(point, {dr, dc}, l, fun)]
    end
  end

  def in_bounds({r, c}, l) when r >= 0 and r < l and c >= 0 and c < l, do: {r, c}
  def in_bounds({_, _}, _), do: false

  def c_many_points([], _, _), do: []
  def c_many_points([h | t], l, fun), do: Enum.map(t, &(fun.(h, &1, l))) ++ c_many_points(t, l, fun) |> List.flatten()
end

input_matrix = hd(System.argv) |> File.read!() |> String.split("\n", trim: true) |> Enum.map(&String.split(&1, "", trim: true))

res = for row <- 0..(length(input_matrix) - 1), col <- 0..(length(input_matrix) - 1), reduce: %{} do
  acc -> Map.update(acc, Enum.at(Enum.at(input_matrix, row), col), [{row, col}], fn positions -> [{row, col} | positions] end)
end |> Map.delete(".")
c_res = fn f -> Enum.reduce(res, [], fn {_, v}, acc ->acc ++ Day8.c_many_points(v, length(input_matrix), f) end) |> Enum.uniq() |> Enum.count() end

IO.inspect({c_res.(&Day8.c_points/3)-1, c_res.(&Day8.c_points_2/3)})
