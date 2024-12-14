defmodule Day14 do

  def c_point({nx, ny, dx, dy}, {max_x, max_y}), do: {c_point(nx, max_x), c_point(ny, max_y), dx, dy}
  def c_point(n, m) do
    cond do
      n < 0 -> m + rem(n, m)
      n >= m -> rem(n, m)
      true -> n
    end
  end

  def get_quadrant({x, y}, {max_x, max_y}),
    do: {get_quadrant(x, max_x, {:left, :right}), get_quadrant(y, max_y, {:top, :bottom})}
  def get_quadrant(n, m, {s1, s2}) do
    cond do
      n < div(m, 2) -> s1
      n > div(m, 2) -> s2
      true -> :center
    end
  end

  def try_next({nx, ny, dx, dy}, {max_x, max_y}), do: c_point({nx + dx, ny + dy, dx, dy}, {max_x, max_y})

  def find_solution(list, _, count, count), do: list
  def find_solution(list, max, count, m_count) do
    n_list = Enum.map(list, & try_next(&1, max))
    verify_solution(n_list |> Enum.map(fn {nx, ny, _, _} -> {nx, ny} end))
      && count + 1 || find_solution(n_list, max, count + 1, m_count)
  end

  def verify_solution(list), do: Enum.any?(list, fn {x, y} -> starts_christmas_tree?({x, y}, list) end)

  def starts_christmas_tree?({x, y}, list) do
    next_two = [{x + 1, y + 1}, {x - 1, y + 1}]
    next_following_two = [{x + 2, y + 2}, {x - 2, y + 2}]
    next_following_following_two = [{x + 3, y + 3}, {x - 3, y + 3}]
    Enum.all?(next_two, fn {nx, ny} -> Enum.member?(list, {nx, ny}) end)
    && Enum.all?(next_following_two, fn {nx, ny} -> Enum.member?(list, {nx, ny}) end)
    && Enum.all?(next_following_following_two, fn {nx, ny} -> Enum.member?(list, {nx, ny}) end)
  end
end

input = hd(System.argv()) |> File.read!() |> String.split("\n", trim: true)
|> Enum.map(fn line -> Regex.scan(~r/\-?\d+/, line) |> List.flatten() |> Enum.map(&String.to_integer/1) |> List.to_tuple() end)

{Day14.find_solution(input, {101, 103}, 0, 100) |> Enum.map(fn {x, y, _, _} -> Day14.get_quadrant({x, y}, {101, 103}) end)
|> Enum.frequencies() |> Enum.filter(fn {{x, y}, _} -> x != :center && y != :center end)
|> Enum.reduce(1, fn {{_, _}, count}, acc -> acc * count end), Day14.find_solution(input, {101, 103}, 0, :infinity)} |> IO.inspect()
