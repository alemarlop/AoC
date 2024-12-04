defmodule Day4 do
  @offsets [{-1, -1}, {0, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {0, 1}, {1, 1}]
  @cross_offsets_left [{-1, -1}, {1, 1}]
  @cross_offsets_right [{1, -1}, {-1, 1}]

  def calculate_words(map, {x,y}, "X"),
    do: Enum.map(@offsets, fn {dx, dy} -> calculate_words(map, {x + dx, y + dy}, "M", {dx, dy}) end) |> Enum.sum()
  def calculate_words(_, _, _), do: 0
  def calculate_words(map, {x,y}, expected, {dx,dy}) do
    actual = get_position(map, {x, y}, length(map) - 1)
    case ^actual = expected do
      "M" -> calculate_words(map, {x + dx, y + dy}, "A", {dx, dy})
      "A" -> calculate_words(map, {x + dx, y + dy}, "S", {dx, dy})
      "S" -> 1
    end
  rescue
    _ -> 0
  end

  def calculate_cross_words(map, {x,y}, "A") do
    left = Enum.map(@cross_offsets_left, fn {dx, dy} -> get_position(map, {x + dx, y + dy}, length(map) - 1) end)
    right = Enum.map(@cross_offsets_right, fn {dx, dy} -> get_position(map, {x + dx, y + dy}, length(map) - 1) end)
    validate_cross(Enum.join(left, "")) && validate_cross(Enum.join(right, "")) && 1 || 0
  end
  def calculate_cross_words(_, _, _), do: 0

  def validate_cross(str) when str == "SM" or str == "MS", do: true
  def validate_cross(_), do: false

  def get_position(_, {x, y}, sq_size) when x < 0 or y < 0 or x > sq_size or y > sq_size, do: :none
  def get_position(map, {x, y}, _), do: Enum.at(Enum.at(map, x), y)
end

input = hd(System.argv)
|> File.read!
|> String.split("\n")
|> Enum.map(&String.split(&1, "", trim: true))

sq_size = length(input) - 1
p1 = (for i <- 0..sq_size, j <- 0..sq_size, do: {i, j, Day4.calculate_words(input, {i,j}, Enum.at(Enum.at(input, i), j))})
|> Enum.reduce(0, fn {_, _, words}, acc -> acc + words end)

p2 = (for i <- 0..sq_size, j <- 0..sq_size, do: {i, j, Day4.calculate_cross_words(input, {i,j}, Enum.at(Enum.at(input, i), j))})
|> Enum.reduce(0, fn {_, _, words}, acc -> acc + words end)

IO.inspect({p1,p2})
