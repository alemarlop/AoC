defmodule Day1 do
  def sort_many({l1, l2}), do: {Enum.sort(l1), Enum.sort(l2)}

  def run do
    input = format()
    {p1(input), p2(input)}
  end

  def format do
    hd(System.argv())
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Stream.map(fn x -> String.split(x, "\s\s\s") end)
    |> Stream.map(fn [x, y] -> {String.to_integer(x), String.to_integer(y)} end)
    |> Enum.unzip()
  end

  def p1(input) do
    {l1s, l2s} = sort_many(input)
    Stream.zip(l1s, l2s) |> Enum.reduce(0, fn {x, y}, acc -> acc + abs(y - x) end)
  end

  def p2({l1, l2}) do
    freq_map = Enum.frequencies(l2)
    Enum.reduce(l1, 0, fn x, acc -> acc + x * Map.get(freq_map, x, 0) end)
  end
end

IO.inspect Day1.run()
