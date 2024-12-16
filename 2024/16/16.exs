defmodule Day16 do
  @directions %{
    :up => {0, -1},
    :down => {0, 1},
    :left => {-1, 0},
    :right => {1, 0}
  }

  @turns %{
    {:up, :left} => {-1, 0},
    {:up, :right} => {1, 0},
    {:down, :left} => {1, 0},
    {:down, :right} => {-1, 0},
    {:left, :left} => {0, 1},
    {:left, :right} => {0, -1},
    {:right, :left} => {0, -1},
    {:right, :right} => {0, 1}
  }

  @turns_direction %{
    {:up, :left} => :left,
    {:up, :right} => :right,
    {:down, :left} => :right,
    {:down, :right} => :left,
    {:left, :left} => :down,
    {:left, :right} => :up,
    {:right, :left} => :up,
    {:right, :right} => :down
  }

  :ets.new(:visited_cache, [:named_table, :public, :set])

  def get_cache({x, y, dir}) do
    case :ets.lookup(:visited_cache, {x, y, dir}) do
      [] -> nil
      [{_, points}] -> points
    end
  end

  def put_cache({x, y, dir}, points), do: :ets.insert(:visited_cache, {{x, y, dir}, points})

  def run(maze, {x, y}, direction, points, visited) do
    cached = case get_cache({x, y, direction}) do
      nil -> false
      x -> x < points
    end

    case cached do
      true -> {false, []}
      false ->
        put_cache({x, y, direction}, points)
        result = Enum.map([:forward, :left, :right], fn dir -> run(maze, {x, y}, direction, dir, points, visited) end)
        |> Enum.filter(fn {success, _} -> success end)
        min = Enum.map(result, fn {points, _} -> points end) |> Enum.min()
        all_mins = [Enum.filter(result, fn {points, _} -> points == min end) |> Enum.map(& elem(&1, 1))] |> List.flatten()
        {min, all_mins}
    end

  rescue
    _ -> {false, []}
  end

  def run(maze, {x, y}, direction, :forward, points, visited) do
    {dx, dy} = Map.get(@directions, direction)
    {new_x, new_y} = {x + dx, y + dy}
    case elem(maze, new_y) |> elem(new_x) do
      "." -> run(maze, {new_x, new_y}, direction, points + 1, [{x, y} | visited])
      "E" -> {points + 1, [{x, y} | visited]}
      _ -> {false, []}
    end
  end

  def run(maze, {x, y}, direction, turn, points, visited) do
    {dx, dy} = Map.get(@turns, {direction, turn})
    {new_x, new_y} = {x + dx, y + dy}
    case elem(maze, new_y) |> elem(new_x) do
      "." -> run(maze, {new_x, new_y}, Map.get(@turns_direction, {direction, turn}), points + 1001, [{x, y} | visited])
      "E" -> {points + 1001, [{x, y} | visited]}
      _ -> {false, []}
    end
  end
end

maze = hd(System.argv) |> File.read!() |> String.split("\n")
|> Enum.map(& String.graphemes(&1) |> List.to_tuple())  |> List.to_tuple()

{p1, p2_l} = Day16.run(maze, {1, length(Tuple.to_list(maze)) - 2}, :right, 0, [])
IO.inspect({p1, 1 + (p2_l |> Enum.uniq() |> Enum.count())})
