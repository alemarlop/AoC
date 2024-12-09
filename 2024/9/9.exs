defmodule Day9 do
  def parse(n), do: parse([], n, 0, :even)
  def parse(p, [], _, _), do: p
  def parse(p, [times | t], n, :even), do: parse(p ++ List.duplicate(n, times), t, n+1, :odd)
  def parse(p, [times | t], n, :odd), do: parse(p ++ List.duplicate(".", times), t, n, :even)

  def parse_2(n), do: parse_2([], n, 0, :even)
  def parse_2(p, [], _, _), do: p
  def parse_2(p, [times | t], n, :even), do: parse_2(p ++ [{:file, n, times}], t, n+1, :odd)
  def parse_2(p, [times | t], n, :odd), do: parse_2(p ++ [{:blank, ".", times}], t, n, :even)

  def compact_list(res, _, []), do: res
  def compact_list(res, [], _), do: res
  def compact_list(res, [{".", idx} | t] = l, [{num, _} | tr]) do
    case num == "." do
      true -> compact_list(res, l, tr)
      false -> compact_list([{num, idx} | res], t, tr)
    end
  end
  def compact_list(res, [{num, idx} | t], rl), do: compact_list([{num, idx}| res], t, rl)

  def compact_list_2(list, []), do: list
  def compact_list_2(list, [{{:blank, _, _},_ }| t]), do: compact_list_2(list, t)
  def compact_list_2(list, {:file, id, size}) do
    case Enum.find(list, fn {{type, _, b_size}, _} -> type == :blank && b_size >= size end) do
      nil -> list
      {{:blank, ".", b_size}, idx} -> case b_size - size do
        0 -> List.replace_at(list, idx, {{:file, id, size}, idx})
        n -> List.replace_at(list, idx, [{{:file, id, size}, idx}, {{:blank, ".", n}, idx}]) |> List.flatten()
      end
    end |> recalculate_indexes()
  end

  def recalculate_indexes(list), do: Enum.map(list, fn {{type, id, size}, _} -> {type, id, size} end) |> Enum.with_index()

  def format_list([], _), do: []
  def format_list([{{_, id, size}, _} | t], visited) do
    case MapSet.member?(visited, id) do
      false -> [List.duplicate(id, size)]
      true -> [List.duplicate(".", size)]
    end ++ format_list(t, MapSet.put(visited, id))
  end
end

input_contents = hd(System.argv) |> File.read!() |> String.graphemes() |> Enum.map(&String.to_integer/1)

p_list = Day9.parse(input_contents) |> Enum.with_index()

res = Day9.compact_list([], p_list, Enum.reverse(p_list)) |> Enum.reverse()
count = Enum.filter(p_list, fn {num, _} -> num != "." end) |> length
p1 = Enum.reduce(Enum.take(res, count), 0, fn {num, idx}, acc -> acc + num * idx end)

p2_list = Day9.parse_2(input_contents) |> Enum.with_index()
p2 = Enum.reverse(p2_list) |> Enum.filter(fn {{type, _, _}, _} -> type == :file end)
|> Enum.reduce(p2_list, fn {b, _}, acc -> Day9.compact_list_2(acc, b) end)
|> Day9.format_list( MapSet.new([])) |> List.flatten() |> Enum.with_index()
|> Enum.reduce(0, fn {num, idx}, acc -> acc + (num != "." && num * idx || 0) end)

IO.inspect({p1, p2})
