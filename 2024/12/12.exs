defmodule Day12 do
  @offsets [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]
  @straight_directions %{
    :up => {-1, 0},
    :down => {1, 0},
    :left => {0, -1},
    :right => {0, 1}
  }
  @turns %{
    :up => [{:left, 0, -1}, {:right, 0, 1}, {:left , -1, -1}, {:right, -1, 1}],
    :down => [{:left, 0, -1}, {:right, 0, 1}, {:left , 1, -1}, {:right, 1, 1}],
    :left => [{:down, 1, 0}, {:up, -1, 0}, {:down , 1, -1}, {:up, -1, -1}],
    :right => [{:up, -1, 0}, {:down, 1, 0}, {:up , -1, 1}, {:down, 1, 1}]
  }

  def get_sizes(_, [], _, v_b, v_p), do: {Enum.count(v_b) * Enum.count(v_p), MapSet.new(v_p), v_b}
  def get_sizes(matrix, [{row, column} = coordinates | t], value, visited_bounds, visited_points) do
    case Enum.member?(visited_points, coordinates) do
      true -> get_sizes(matrix, t, value, visited_bounds, visited_points)
      false ->
        case verify_bounds(matrix, coordinates) && Enum.at(Enum.at(matrix, row), column) == value do
          true -> get_sizes(matrix, t ++ to_check(coordinates), value, visited_bounds, [coordinates | visited_points])
          false -> get_sizes(matrix, t, value, [coordinates | visited_bounds], visited_points)
        end
      end
  end

  def duplicate_matrix([]), do: []
  def duplicate_matrix([row | t]) do
    row = duplicate_matrix_row(row)
    [row, row] ++ duplicate_matrix(t)
  end
  def duplicate_matrix_row([]), do: []
  def duplicate_matrix_row([h | t]) do
    [h, h] ++ duplicate_matrix_row(t)
  end

  def reduce_perimeter([], result), do: result
  def reduce_perimeter([_ | _] = l, result) do
    {starting_direction, starting_point} = calculate_inital_direction(l)
    reduce_perimeter([starting_point | List.delete(l, starting_point)], starting_direction, result + 1)
  end
  def reduce_perimeter([_ | t] = l, direction, count) do
    possibilities = calculate_turn(direction, l) ++ [calculate_srtaight(direction, l)]
    maybe_point = Enum.find(possibilities, fn {_, pos} -> not is_nil(pos) end)
    case maybe_point do
      nil -> reduce_perimeter(t, count)
      {n_direction, point} when n_direction == direction -> reduce_perimeter([point | List.delete(t, point)], direction, count)
      {n_direction, point} -> reduce_perimeter([point | List.delete(t, point)], n_direction, count + 1)
    end
  end

  def calculate_srtaight(direction, [{row, column} | t]) do
    {d_row, d_column} = Map.get(@straight_directions, direction)
    {direction, Enum.find(t, fn {r, c} -> {r, c} == {row + d_row, column + d_column} end)}
  end

  def calculate_turn(direction, [_ | _] = l) do
    list = Map.get(@turns, direction)
    Enum.map(list, fn pos -> calculate_turn(l, pos) end)
  end

  def calculate_turn([{row, column} | t], {n_direction, d_row, d_col}) do
    {n_direction, Enum.find(t, fn {r, c} -> r == row + d_row && c == column + d_col end)}
  end

  def calculate_inital_direction([{row, column} | t] = l) do
    cond do
      Enum.member?(t, {row-1, column}) -> {:up, {row, column}}
      Enum.member?(t, {row+1, column}) -> {:down, {row, column}}
      Enum.member?(t, {row, column-1}) -> {:left, {row, column}}
      Enum.member?(t, {row, column+1}) -> {:right, {row, column}}
      true ->IO.inspect(l) && calculate_inital_direction(t ++ [{row, column}])
    end
  end

  def list_difference(list1, list2) do
    Enum.reject(list1, fn x -> Enum.member?(list2, x) end)
  end

  def to_check({row, column}, set\\@offsets), do: Enum.map(set, fn {r, c} -> {row + r, column + c} end)
  def verify_bounds(matrix, {row, column}), do: row >= 0 and row < length(matrix) and column >= 0 and column < length(matrix)
end

matrix = hd(System.argv) |> File.read!() |> String.split("\n", trim: true)
|> Enum.map(&String.split(&1, "", trim: true))



p1 = (for row <- 0..(length(matrix) - 1), column <- 0..(length(matrix) - 1), reduce: {MapSet.new([]), 0} do
  {visited, total} ->
    point = Enum.at(Enum.at(matrix, row), column)
    case MapSet.member?(visited, {row, column}) do
      true -> {visited, total}
      false ->
        {_, p_visited, b_visited} = Day12.get_sizes(matrix, [{row, column}], point, [], [])
        {MapSet.union(visited, p_visited), total + MapSet.size(p_visited) * length(b_visited)}
    end
end) |> elem(1)

duplicated = Day12.duplicate_matrix(matrix) |> Day12.duplicate_matrix()

p2 = (for row <- 0..(length(duplicated) - 1), column <- 0..(length(duplicated) - 1), reduce: {MapSet.new([]), 0} do
  {visited, total} ->
    point = Enum.at(Enum.at(duplicated, row), column)
    case MapSet.member?(visited, {row, column}) do
      true -> {visited, total}
      false ->
        {_, p_visited, b_visited} = Day12.get_sizes(duplicated, [{row, column}], point, [], [])
        feqs = Enum.frequencies(b_visited)
        corners = Enum.filter(b_visited, fn {r, c} -> Map.get(feqs, {r, c}) > 1 end)
        b_visited_without_corners = Day12.list_difference(b_visited, corners)
        a = div(MapSet.size(p_visited), 16)
        initial_reduction = Day12.reduce_perimeter(b_visited_without_corners, 0)
        {MapSet.union(visited, p_visited), total + a * initial_reduction}
    end
end) |> elem(1)
IO.inspect({p1, p2})
