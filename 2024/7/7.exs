defmodule Day7 do
  def format([h | [ht | t]]), do: {String.to_integer(h), String.to_integer(ht), Enum.map(t, &String.to_integer/1)}
  def run({n, m, []}, _), do: n == m
  def run({tgt, curr, [h | t]}, funs), do: Enum.any?(funs, fn f -> run({tgt, f.(curr, h), t}, funs) end)
end

input = hd(System.argv) |> File.read!() |> String.split("\n") |> Enum.map(fn line -> Regex.scan(~r/\d+/, line) |> List.flatten() |> Day7.format() end)

result = fn ipt, funs -> Enum.reduce(ipt, 0, fn {tgt, _, _} = arg, acc -> acc + (Day7.run(arg, funs) && tgt || 0) end) end
{result.(input, [&+/2, &*/2]), result.(input, [&+/2, &*/2, fn a, b -> String.to_integer("#{a}#{b}") end])} |> IO.inspect()
