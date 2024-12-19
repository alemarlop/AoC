defmodule Day18 do
  @moves [[0, 1], [0, -1], [1, 0], [-1, 0]]

  :ets.new(:visited_cache, [:named_table, :public, :set])

  def get_cache({x, y}) do
    case :ets.lookup(:visited_cache, {x, y}) do
      [] -> nil
      [{_, points}] -> points
    end
  end

  def reset_cache do
    :ets.delete(:visited_cache)
    :ets.new(:visited_cache, [:named_table, :public, :set])
  end

  def put_cache({x, y}, points), do: :ets.insert(:visited_cache, {{x, y}, points})

  def run_n_bytes(list, length, [h | t]) do
    reset_cache()
    run_n_bytes(list, length, h)
    run_n_bytes(list, length, t)
  end
  def run_n_bytes(list, length, bytes) when is_integer(bytes) do
    map = draw_map(list, length, bytes)
    case find_path_bfs(map, {length,length}, 0, [{0, 0}], length) do
      :false -> raise Enum.at(list, bytes - 1)
      x -> x
    end
  end

  def find_path_bfs(_, _, _, [], _), do: :false
  def find_path_bfs(map, target, steps, coordinates, length) do
    coordinates = Enum.uniq(coordinates) |> Enum.sort_by(fn {x1, y1} -> calculate_distance(target, {x1, y1}) end) |> Enum.take(1_000)
    points = Enum.map(coordinates, fn coo -> expand_point(map, coo, length, steps) end) |> List.flatten()
    find_path_bfs(map, target, steps + 1, points, length)
  rescue
    err -> case err do
      %{message: message} -> message
    end
  end

  def expand_point(_, {length, length}, length, steps), do: raise "#{steps}"
  def expand_point(map, {x, y}, length, steps) do
    put_cache({x, y}, steps)
    Enum.map(@moves, fn [dx, dy] -> {x + dx, y + dy} end)
    |> Enum.filter(fn {x, y} -> is_valid?(map, {x, y}, length) end)
  end

  def is_valid?(map, {x, y}, length) do
    x >= 0 && x <= length && y >= 0 && y <= length && get_cache({x, y}) == nil && elem(elem(map, y), x) != "#"
  end

  def calculate_distance({x1, y1}, {x2, y2}) do
    abs(x1 - x2) + abs(y1 - y2)
  end

  def draw_map(points, max_length, bytes) do
    list = draw_matrix(0, Enum.take(points, bytes) |> MapSet.new(), max_length)
    Enum.map(list, fn row -> List.to_tuple(row) end)
    |> List.to_tuple()
  end

  def draw_matrix(y, _, max_y) when y > max_y, do: []
  def draw_matrix(y, points, max_y) do
    [draw_matrix_row(y, 0, points, max_y) | draw_matrix(y + 1, points, max_y)]
  end

  def draw_matrix_row(_, x, _, max_x) when x > max_x, do: []
  def draw_matrix_row(y, x, points, max_x) do
    [MapSet.member?(points, [x, y]) && "#" || "." | draw_matrix_row(y, x + 1, points, max_x)]
  end
end

list = hd(System.argv())
|> File.read!()
|> String.split("\n")
|> Enum.map(fn x -> String.split(x, ",") |> Enum.map(&String.to_integer/1) end)

Day18.draw_map(list, 70, 1024) |>
Day18.find_path_bfs({70, 70}, 0, [{0, 0}], 70) |> IO.inspect()
Day18.run_n_bytes(list, 70, Range.to_list(1024..length(list))) |> IO.inspect()
