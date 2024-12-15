Code.require_file("15_2.exs", __DIR__)
defmodule Day15 do

  def rotate_clockwise(matrix) do
    matrix
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.map(&Enum.reverse/1)
  end

  def rotate_counter_clockwise(matrix) do
    matrix
    |> Enum.map(&Enum.reverse/1)
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
  end

  def find_starting_position(map) do
    map
    |> Enum.with_index()
    |> Enum.map(fn {row, y} ->
      row
      |> Enum.with_index()
      |> Enum.map(fn {cell, x} -> {cell, {x, y}} end)
    end)
    |> List.flatten()
    |> Enum.find(fn {cell, _} -> cell == "@" end)
    |> elem(1)
  end

  def push_boxes_up(map, _, {dx, dy}) do
    rotated = rotate_clockwise(map)
    {_, _, rotated} = push_boxes_right(rotated, find_starting_position(rotated), {dy, dx})
    rotated_1 = rotate_counter_clockwise(rotated)
    {nx, ny} = find_starting_position(rotated_1)
    {nx, ny, rotated_1}
  end

  def push_boxes_down(map, _, {dx, dy}) do
    rotated = rotate_clockwise(map)
    {_, _, rotated} = push_boxes_left(rotated, find_starting_position(rotated), {dy, dx})
    rotated_1 = rotate_counter_clockwise(rotated)
    {nx, ny} = find_starting_position(rotated_1)
    {nx, ny, rotated_1}
  end

  def push_boxes_right(map, {x, y}, {dx, dy}) do
    {pre_map, post_map} = {Enum.slice(map, 0..y-1), Enum.slice(map, y+1..length(map))}
    row = Enum.at(map, y)
    {pre, post} = {Enum.slice(row, 0..x-1), Enum.slice(row, x..length(row))}
    wall_i = Enum.find_index(post, fn cell -> cell == "#" end)
    slot_i = Enum.find_index(post, fn cell -> cell == "." end)
    case slot_i && wall_i > slot_i || false do
      false -> {x, y, map}
      _ -> {x + dx, y + dy, pre_map ++ [pre ++ [Enum.at(post, slot_i) | List.delete_at(post, slot_i)]] ++ post_map}
    end
  end

  def push_boxes_left(map, {x, y}, {dx, dy}) do
    {pre_map, post_map} = {Enum.slice(map, 0..y-1), Enum.slice(map, y+1..length(map))}
    row = Enum.at(map, y)
    {pre, post} = {Enum.slice(row, 0..x) |> Enum.reverse(), Enum.slice(row, x+1..length(row))}
    wall_i = Enum.find_index(pre, fn cell -> cell == "#" end)
    slot_i = Enum.find_index(pre, fn cell -> cell == "." end)
    case slot_i && wall_i > slot_i || false  do
      false -> {x, y, map}
      _ -> {x + dx, y + dy, pre_map ++ [Enum.reverse([Enum.at(pre, slot_i) | List.delete_at(pre, slot_i)]) ++ post] ++ post_map}
    end
  end

  def move("^", map, {x, y}), do: push_boxes_up(map, {x, y}, {0, -1})
  def move("v", map, {x, y}), do: push_boxes_down(map, {x, y}, {0, 1})
  def move(">", map, {x, y}), do: push_boxes_right(map, {x, y}, {1, 0})
  def move("<", map, {x, y}), do: push_boxes_left(map, {x, y}, {-1, 0})

  def move_all([], map, _), do: map
  def move_all([h | t], map, position) do
    {x, y, map} = move(h, map, position)
    move_all(t, map, {x, y})
  end

  def calculate_coordinates_sum(map, moves) do
    map = Day15.move_all(moves, map, Day15.find_starting_position(map))
    for x <- 0..length(map) - 1, y <- 0..length(map) - 1, reduce: 0 do
      acc -> Enum.at(Enum.at(map, y), x) == "O" && acc + x + 100 * y || acc
    end
  end
end

[map, moves] = hd(System.argv())
|> File.read!()
|> String.split("\n\n", trim: true)

map = String.split(map, "\n", trim: true) |> Enum.map(&String.graphemes/1)
moves = String.split(moves, "\n", trim: true) |> Enum.join() |> String.graphemes()

IO.inspect({Day15.calculate_coordinates_sum(map, moves), Day15.Part2.calculate_coordinates_sum(map, moves)})
