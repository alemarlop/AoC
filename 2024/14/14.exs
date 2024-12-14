defmodule Day14 do
  def c_point({nx, ny, dx, dy}, {max_x, max_y}), do: {c_point(nx, max_x), c_point(ny, max_y), dx, dy}
  def c_point(n, m) when n < 0, do: m + rem(n, m)
  def c_point(n, m) when n >= m, do: rem(n, m)
  def c_point(n, _), do: n

  def get_quadrant({x, y}, {max_x, max_y}),
    do: {get_quadrant(x, max_x, {:left, :right}), get_quadrant(y, max_y, {:top, :bottom})}
  def get_quadrant(n, m, {s1, _}) when n < div(m, 2), do: s1
  def get_quadrant(n, m, {_, s2}) when n > div(m, 2), do: s2
  def get_quadrant(_, _, _), do: :center

  def try_next({nx, ny, dx, dy}, {max_x, max_y}), do: c_point({nx + dx, ny + dy, dx, dy}, {max_x, max_y})

  def find_solution(list, _, count, count), do: list
  def find_solution(list, max, count, m_count) do
    n_list = Enum.map(list, & try_next(&1, max))
    verify_solution(n_list |> Enum.map(fn {nx, ny, _, _} -> {nx, ny} end))
      && count + 1 || find_solution(n_list, max, count + 1, m_count)
  end

  def verify_solution(list), do: Enum.any?(list, fn {x, y} -> starts_christmas_tree?({x, y}, list) end)

  def starts_christmas_tree?({x, y}, list) do
    [{x + 1, y + 1}, {x - 1, y + 1}, {x + 2, y + 2}, {x - 2, y + 2}, {x + 3, y + 3}, {x - 3, y + 3}]
    |> Enum.all?(& Enum.member?(list, &1))
  end
end

input = hd(System.argv()) |> File.read!() |> String.split("\n", trim: true)
|> Enum.map(fn line -> Regex.scan(~r/\-?\d+/, line) |> List.flatten() |> Enum.map(&String.to_integer/1) |> List.to_tuple() end)

{Day14.find_solution(input, {101, 103}, 0, 100) |> Enum.map(fn {x, y, _, _} -> Day14.get_quadrant({x, y}, {101, 103}) end)
|> Enum.frequencies() |> Enum.filter(fn {{x, y}, _} -> x != :center && y != :center end)
|> Enum.reduce(1, fn {{_, _}, count}, acc -> acc * count end), Day14.find_solution(input, {101, 103}, 0, :infinity)} |> IO.inspect()
