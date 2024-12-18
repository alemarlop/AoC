defmodule Day15 do

  def rotate_clockwise(matrix),
    do: matrix |> Enum.zip() |> Enum.map(&Tuple.to_list/1) |> Enum.map(&Enum.reverse/1)

  def rotate_counter_clockwise(matrix),
    do: matrix |> Enum.map(&Enum.reverse/1) |> Enum.zip() |> Enum.map(&Tuple.to_list/1)

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

  def move("^", map, {x, y}, :p1), do: push_boxes_up(map, {x, y}, {0, -1})
  def move("v", map, {x, y}, :p1), do: push_boxes_down(map, {x, y}, {0, 1})
  def move("^", map, {x, y}, :p2), do: push_boxes_vertical(map, {x, y}, -1)
  def move("v", map, {x, y}, :p2), do: push_boxes_vertical(map, {x, y}, 1)
  def move(">", map, {x, y}, _), do: push_boxes_right(map, {x, y}, {1, 0})
  def move("<", map, {x, y}, _), do: push_boxes_left(map, {x, y}, {-1, 0})

  def move_all([], map, _, _), do: map
  def move_all([h | t], map, position, p) do
    {x, y, map} = move(h, map, position, p)
    move_all(t, map, {x, y}, p)
  end

  def calculate_coordinates_sum(map, moves) do
    map = move_all(moves, map, Day15.find_starting_position(map), :p1)
    for x <- 0..length(map) - 1, y <- 0..length(map) - 1, reduce: 0 do
      acc -> Enum.at(Enum.at(map, y), x) == "O" && acc + x + 100 * y || acc
    end
  end

  def calculate_coordinates_sum_2(map, moves) do
    map = build_map(map)
    map = move_all(moves, map, find_starting_position(map), :p2)
    for x <- 0..(length(map) - 1) * 2, y <- 0..length(map) - 1, reduce: 0 do
      acc -> Enum.at(Enum.at(map, y), x) == "[" && acc + x + 100 * y || acc
    end
  end

  def build_map(map), do: Enum.map(map, &build_map_row/1)
  def build_map_row([]), do: []
  def build_map_row([h | t]) do
    case h do
      "O" -> ["[", "]"]
      "@" -> ["@", "."]
      _ -> [h, h]
    end ++ build_map_row(t)
  end

  def can_move_box?(map, {lx, ly}, {rx, ry}, dy) do
    left_content = Enum.at(Enum.at(map, ly + dy), lx)
    right_content = Enum.at(Enum.at(map, ry + dy), rx)
    case {left_content, right_content} do
      {".", "."} -> {true, [{lx, ly}, {rx, ry}]}
      {n, m} when n == "#" or m == "#" -> {false, []}
      {"[", "]"} -> {possible, boxes} = can_move_box?(map, {lx, ly + dy}, {rx, ry + dy}, dy)
        {possible, boxes ++ [{lx, ly}, {rx, ry}]}
      {"]", "."} -> {possible, boxes} = can_move_box?(map, {lx - 1, ly + dy}, {lx, ly + dy}, dy)
        {possible, boxes ++ [{lx, ly}, {rx, ry}]}
      {".", "["} -> {possible, boxes} = can_move_box?(map, {rx, ry + dy}, {rx + 1, ry + dy}, dy)
        {possible, boxes ++ [{lx, ly}, {rx, ry}]}
      {"]", "["} ->
        {possible_l, boxes_l} = can_move_box?(map, {lx - 1, ly + dy}, {lx, ly + dy}, dy)
        {possible_r, boxes_r} = can_move_box?(map, {rx, ry + dy}, {rx + 1, ry + dy}, dy)
        {possible_l && possible_r, boxes_l ++ boxes_r ++ [{lx, ly}, {rx, ry}]}
    end
  end

  def push_boxes_vertical(map, {x, y}, dy) do
    upper_element = Enum.at(Enum.at(map, y + dy), x)
    {should_move, points} = case upper_element do
      "#" -> {false, []}
      "]" -> can_move_box?(map, {x - 1, y + dy}, {x, y + dy}, dy)
      "[" -> can_move_box?(map, {x, y + dy}, {x + 1, y + dy}, dy)
      "." -> {true, []}
    end
    case should_move do
      false -> {x, y, map}
      true -> {x, y + dy,
        (n_map = move_boxes(map, points, dy)) && move_box(n_map, n_map, {x, y}, dy)
      }
    end
  end

  def move_boxes(map, points, dy),
    do: move_all_boxes(map, map, Enum.sort_by(points, fn {_, y} -> y * dy * -1 end), dy)

  def move_all_boxes(_, map, [], _), do: map
  def move_all_boxes(o_map, map, [point | t], dy),
    do: move_all_boxes(o_map, move_box(o_map, map, point, dy), t, dy)

  def move_box(o_map, map, {x, y}, dy) do
    row_i = Enum.at(map, y)
    row_i_1 = Enum.at(map, y + dy)
    row_i = List.replace_at(row_i, x, ".")
    row_i_1 = List.replace_at(row_i_1, x, Enum.at(Enum.at(o_map, y), x))
    map = List.replace_at(map, y, row_i)
    List.replace_at(map, y + dy, row_i_1)
  end

end

[map, moves] = hd(System.argv())
|> File.read!()
|> String.split("\n\n", trim: true)

map = String.split(map, "\n", trim: true) |> Enum.map(&String.graphemes/1)
moves = String.split(moves, "\n", trim: true) |> Enum.join() |> String.graphemes()

IO.inspect({Day15.calculate_coordinates_sum(map, moves), Day15.calculate_coordinates_sum_2(map, moves)})
