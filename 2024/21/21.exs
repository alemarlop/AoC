defmodule Day21 do
  @numpad_coordinates %{
    7 => {0, 0}, 8 => {1, 0}, 9 => {2, 0}, 4 => {0, 1},
    5 => {1, 1}, 6 => {2, 1}, 1 => {0, 2}, 2 => {1, 2},
    3 => {2, 2}, 0 => {1, 3}, :A => {2, 3}
  }
  @valid_numpad_coordinates [
    {0, 0}, {1, 0}, {2, 0}, {0, 1}, {1, 1}, {2, 1},
    {0, 2}, {1, 2}, {2, 2}, {1, 3}, {2, 3}
  ]

  @offset_ops [{0, 1}, {1, 0}, {0, -1}, {-1, 0}]
  @offset_keys %{{0, 1} => "v", {1, 0} => ">", {0, -1} => "^", {-1, 0} => "<"}

  @optimal_paths %{
    {"A", "^"} => ["<", "A"], {"A", ">"} => ["v", "A"], {"A", "v"} => ["<", "v", "A"], {"A", "<"} => ["v", "<", "<", "A"], {"A", "A"} => ["A"],
    {"<", "A"} => [">", ">", "^", "A"], {"<", ">"} => [">", ">", "A"], {"<", "v"} => [">", "A"], {"<", "<"} => ["A"], {"<", "^"} => [">", "^", "A"],
    {"^", "A"} => [">", "A"], {"^", ">"} => ["v", ">", "A"], {"^", "v"} => ["v", "A"], {"^", "<"} => ["v", "<", "A"], {"^", "^"} => ["A"],
    {">", "A"} => ["^", "A"], {">", ">"} => ["A"], {">", "v"} => ["<", "A"], {">", "<"} => ["<", "<", "A"], {">", "^"} => ["<", "^", "A"],
    {"v", "A"} => ["^", ">", "A"], {"v", ">"} => [">", "A"], {"v", "v"} => ["A"], {"v", "<"} => ["<", "A"], {"v", "^"} => ["^", "A"],
  }

  def apply_n(seq, 0), do: seq
  def apply_n(seq, keyboards), do: apply_n(c_1(["A" | seq]), keyboards - 1)

  def calculate_initial(_list, last \\ :A, results \\ [""])
  def calculate_initial([], _last, results), do: results
  def calculate_initial([h | t], last, results) do
    from = @numpad_coordinates[last]
    to = @numpad_coordinates[h]
    new_possibilities = calculate_paths(from, to, [], :A) |> List.flatten()
    new_results = Enum.flat_map(results, fn res -> Enum.map(new_possibilities, fn np -> res <> np end) end)
    calculate_initial(t, h, new_results)
  end

  def calculate_paths(target, target, partial_result, _last), do: Enum.join(partial_result) <> "A"
  def calculate_paths({fx, fy}, {tx, ty}, partial_result, last_key) do
    current_distance = abs(fx - tx) + abs(fy - ty)

    stream = @offset_ops
    |> Stream.map(fn {dx, dy} -> {fx + dx, fy + dy, @offset_keys[{dx, dy}]} end)
    |> Stream.filter(fn {x, y, _} -> {x, y} in @valid_numpad_coordinates end)
    |> Enum.filter(fn {nx, ny, _} -> abs(nx - tx) + abs(ny - ty) < current_distance end)

    case Enum.find(stream, fn {_x, _y, key} -> key == last_key end) do
      nil -> Enum.map(stream, fn {x, y, key} -> calculate_paths({x, y}, {tx, ty}, partial_result ++ [key], key) end)
      {x, y, key} -> calculate_paths({x, y}, {tx, ty}, partial_result ++ [key], key)
    end
  end

  def c_1([h, next | t]), do: @optimal_paths[{h, next}] ++ c_1([next | t])
  def c_1(_), do: []

  def run_all([], _, _, _), do: 0
  def run_all([h | t], kbs, precalculated, precalculated_depth) do
    numeric_part = Enum.take(h, 3) |> Enum.join() |> String.to_integer()
    run_result = run(h, kbs, precalculated, precalculated_depth)
    numeric_part * run_result + run_all(t, kbs, precalculated, precalculated_depth)
  end

  def run(list, kbs, _precalculated_vals, 0) do
    calculate_initial(list)
    |> Stream.map(& String.split(&1, "", trim: true))
    |> Stream.map(& apply_n(&1, kbs))
    |> Stream.map(&length/1)
    |> Enum.min()
  end

  def run(list, kbs, precalculated_vals, precalculated_depth) do
    calculate_initial(list)
    |> Stream.map(& String.split(&1, "", trim: true))
    |> Stream.map(& apply_n(&1, kbs - precalculated_depth))
    |> Stream.map(&chunk/1)
    |> Stream.map(fn elem -> Enum.map(elem, fn item -> Map.get(precalculated_vals, item) end) end)
    |> Stream.map(&Enum.sum/1)
    |> Enum.min()
  end

  def chunk(l, current \\ [])
  def chunk([], _current), do: []
  def chunk(["A" | t], current), do: [current ++ ["A"] | chunk(t, [])]
  def chunk([h | t], current), do: chunk(t, current ++ [h])

  def precaclulate(num),
    do: Enum.reduce(Map.values(@optimal_paths), %{}, fn elem, acc -> Map.put(acc, elem, length(apply_n(elem, num))) end)
end

input = System.argv() |> hd() |> File.read!() |> String.split("\n", trim: true)
|> Enum.map(fn line -> String.split(line, "", trim: true) |> Enum.map(fn "A" -> :A; num -> String.to_integer(num) end) end)
precalulated_depth = 14
precalulated = Day21.precaclulate(precalulated_depth)
p0 = Day21.run_all(input, 2, precalulated, 0)
p1 = Day21.run_all(input, 25, precalulated, precalulated_depth)
IO.inspect({p0, p1})
