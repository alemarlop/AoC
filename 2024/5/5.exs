defmodule Day5 do
  def valid?(_, _, []), do: true
  def valid?(r_map, previous, [current | tail]),
    do: MapSet.intersection(previous, Map.get(r_map, current, MapSet.new([]))) == MapSet.new()
      && valid?(r_map, MapSet.put(previous, current), tail) || current

  def rearrange(r_map, list, i) when is_integer(i), do: rearrange(r_map, [i | List.delete(list, i)], valid?(r_map, MapSet.new([]), list))
  def rearrange(_, l, _), do: l
end

[restrictions, sets] = hd(System.argv) |> File.read!() |> String.split("\n\n")

r_map = restrictions |> String.split("\n") |> Enum.map(fn x -> String.split(x, "|") |> Enum.map(&String.to_integer/1) end)
|> Enum.reduce(%{}, fn [k, v], acc -> Map.update(acc, k, MapSet.new([v]), &MapSet.put(&1, v)) end)
sets = sets |> String.split("\n") |> Enum.map(fn x -> String.split(x, ",") |> Enum.map(&String.to_integer/1) end)

p1 = Enum.filter(sets, fn x -> not is_integer(Day5.valid?(r_map, MapSet.new([]), x)) end)
|> Enum.reduce(0, fn l, acc -> Enum.at(l, div(length(l),2)) + acc end)

p2 = Enum.filter(sets, fn x -> Day5.valid?(r_map, MapSet.new([]), x) |> is_integer() end)
|> Enum.map(fn l -> Day5.rearrange(r_map, l, Day5.valid?(r_map, MapSet.new([]), l)) end)
|> Enum.reduce(0, fn l, acc -> Enum.at(l, div(length(l),2)) + acc end)

IO.inspect({p1, p2})
