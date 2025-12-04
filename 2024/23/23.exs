defmodule Day23 do

  def connections(conns) do
    pc_list = Enum.reduce(conns, MapSet.new(), fn {k, v}, acc -> MapSet.union(acc, MapSet.new([k,v])) end) |> MapSet.to_list()
    Enum.reduce(pc_list, %{}, fn pc, acc ->
      Map.put(acc, pc, Enum.filter(conns, fn {k, v} -> pc in [k, v] end) |> Enum.map(fn {k, v} -> pc == k && v || k end))
    end)
  end

  def group(conns) do
    direct_connections = connections(conns)
    groups = Enum.reduce(Map.keys(direct_connections), MapSet.new(), fn elem, acc ->
      values = Map.get(direct_connections, elem)
      MapSet.union(acc, MapSet.new(group(conns, elem, values) |> Enum.map(& MapSet.new(&1))))
    end) |> MapSet.to_list() |> Enum.map(& MapSet.to_list(&1))

    Enum.filter(groups, & Enum.any?(&1, fn item -> String.starts_with?(item, "t") end)) |> length
  end

  def group(_conns, _elem, []), do: []
  def group(conns, elem, [h | t]) do
    case Enum.filter(t, & are_connected?(conns, &1, h)) do
      [] -> group(conns, elem, t)
      list -> Enum.map(list, & [elem, h, &1]) ++ group(conns, elem, t)
    end
  end

  def find_longest_connection(conns) do
    direct_connections = connections(conns)
    Enum.reduce(Map.keys(direct_connections), [], fn elem, acc -> [find_longest_connection(conns, elem, Map.get(direct_connections, elem)) | acc] end)
    |> Enum.filter(fn {len, words} -> len + 1 == length(words) end)
    |> Enum.sort_by(fn {len, _words} -> len end, :desc)
    |> hd()
    |> elem(1)
    |> Enum.join(",")
  end

  def find_longest_connection(conns, element, list) do
    results = Enum.map(list, fn i1 -> {i1, 1 + Enum.count(list, fn i2 -> are_connected?(conns, i1, i2) end)} end)
    |> Enum.sort_by(fn {_, conns} -> conns end, :desc)
    conn_length = hd(results) |> elem(1)
    results = Enum.filter(results, & elem(&1, 1) == conn_length)
    {conn_length, [element | Enum.map(results, & elem(&1, 0))] |> Enum.sort()}
  end

  defp are_connected?(conns, pc1, pc2),
    do: Enum.any?(conns, fn {k, v} -> (k == pc1 && v == pc2) || (k == pc2 && v == pc1) end)
end

connections = hd(System.argv()) |> File.read!() |> String.split("\n", trim: true)
|> Enum.map(fn line -> String.split(line, "-", trim: true) |> List.to_tuple end)

IO.inspect({Day23.group(connections), Day23.find_longest_connection(connections)})
