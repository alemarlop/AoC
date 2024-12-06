defmodule Day6 do
  @directions %{
    "^" => {-1, 0},
    "v" => {1, 0},
    "<" => {0, -1},
    ">" => {0, 1}
  }

  @rotations %{
    "^" => ">",
    "v" => "<",
    "<" => "^",
    ">" => "v"
  }

  def calculate_variants(["X" | t], done, result), do: calculate_variants(t, done ++ ["."], result ++ [done ++ ["#" | t]])
  def calculate_variants([h | t], done, result), do: calculate_variants(t, done ++ [h], result)
  def calculate_variants([], _, result), do: result

  def get_starting_point(matrix, direction) do
    row = Enum.find_index(matrix, fn r -> Enum.member?(r, direction) end)
    column = Enum.find_index(Enum.at(matrix, row), fn r -> r == direction end)
    {row, column}
  end

  def calculate_movements(matrix, {x, y}, direction, visited, count) do
    case MapSet.member?(visited, {x, y, direction}) do
      true -> :loop
      false -> calculate_movements(matrix, {x, y}, direction, visited, count, :miss)
    end
  end

  def calculate_movements(matrix, {x, y}, direction, visited, count, :miss) do
    {dx, dy} = Map.get(@directions, direction)
    row = Enum.at(matrix, x)
    {new_x, new_y} = calculate_next(matrix, {x, y}, {dx, dy})
    new_row = Enum.at(matrix, new_x)
    new_point = Enum.at(Enum.at(matrix, new_x), new_y)
    MapSet.put(visited, {x, y, direction})
    case new_point do
      "#" -> new_matrix = List.replace_at(matrix, x, List.replace_at(row, y, "X"))
             calculate_movements(new_matrix, {x, y}, Map.get(@rotations, direction), MapSet.put(visited, {x, y, direction}), count)
      _   -> new_matrix = List.replace_at(matrix, new_x, List.replace_at(new_row, new_y, "X"))
             calculate_movements(new_matrix, {new_x, new_y}, direction, MapSet.put(visited, {x, y, "X"}), count + 1)
    end
  rescue
    _ -> matrix
  end

  def calculate_next(matrix, {x, y}, {dx, dy}) when x + dx < 0 or y + dy < 0 or x + dx >= length(matrix) or y + dy >= length(matrix) do
    raise "Out of bounds"
  end
  def calculate_next(_, {x, y}, {dx, dy}), do: {x + dx, y + dy}
end

matrix = hd(System.argv)
|> File.read!() |> String.split("\n", trim: true) |> Enum.map(&String.split(&1, "", trim: true))
{x, y} = Day6.get_starting_point(matrix, "^")
p1_map = Day6.calculate_movements(matrix, {x, y}, "^", MapSet.new([]), 0)

variants = p1_map |> Enum.join("\n") |> String.split("", trim: true) |> Day6.calculate_variants([],[])
p1 = length(variants)
f_variants = Enum.map(variants, fn v -> Enum.chunk_by(v, fn chunk -> chunk == "\n" end) |> Enum.reject(fn chunk -> chunk == ["\n"] end) end)
f_variants = Enum.map(f_variants, fn variant -> List.replace_at(variant, x, List.replace_at(Enum.at(variant, x), y, "^")) end)

p2 = Task.async_stream(f_variants, Day6, :calculate_movements, [{x, y}, "^", MapSet.new([]), 0], max_concurrency: 16)
|> Enum.to_list()
|> Enum.reduce(-1, fn x, acc -> acc + case x do
  {:ok, :loop} -> 1
  _ -> 0 end
end)

IO.inspect({p1, p2})
