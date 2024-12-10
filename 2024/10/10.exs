defmodule Day10 do
  @offsets [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]

  def to_check({row, column}), do: Enum.map(@offsets, fn {r, c} -> {row + r, column + c} end)
  def get_value(matrix, {row, column} = p), do: verify_bounds(matrix, p) && Enum.at(Enum.at(matrix, row), column)

  def verify_bounds(matrix, {row, column}) when row >= 0 and row < length(matrix) and column >= 0 and column < length(matrix),
    do: {row, column}
  def verify_bounds(_, _), do: nil

  def run(matrix, 0, position), do: run(matrix, 0, position, :in_trail)
  def run(_, _, _), do: []
  def run(matrix, 9, position, :in_trail), do: get_value(matrix, position) == 9 && [position] || []
  def run(matrix, to, position, :in_trail) do
    case get_value(matrix, position) do
      val when val == to -> to_check(position) |> Enum.map(fn pos -> run(matrix, to+1, pos, :in_trail) end)  |> List.flatten()
      _ -> []
    end
  end
end

matrix = hd(System.argv) |> File.read!() |> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, "", trim: true) |> Enum.map(fn x -> String.to_integer(x) end))

(for row <- 0..(length(matrix) - 1), column <- 0..(length(matrix) - 1), reduce: {0,0} do
  {p1, p2} -> case Day10.run(matrix, Enum.at(matrix, row) |> Enum.at(column), {row, column}) do
      [] -> {p1, p2}
      res -> {p1 + length(res |> Enum.uniq()), p2 + (Enum.frequencies(res) |> Enum.map(fn {k, v} -> v end) |> Enum.sum())}
    end
end) |> IO.inspect()
