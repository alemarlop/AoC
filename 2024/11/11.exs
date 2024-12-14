# It needs to be executed in a mix project so it can use the Memoize module.
defmodule Day11 do
  use Memoize

  def c_stone(0), do: [1]
  def c_stone(k) do
    digits = to_string(k)
    case rem(String.length(digits), 2) do
      0 -> [String.slice(digits, 0, div(String.length(digits), 2)) |> String.to_integer(),
            String.slice(digits, div(String.length(digits), 2), div(String.length(digits), 2)) |> String.to_integer()]
      _ -> [2024 * k]
    end
  end

  def c_stone(_, 0), do: 1
  defmemo c_stone(k, n), do: c_stone(k) |> Enum.map(&c_stone(&1, n-1)) |> Enum.sum()
end

hd(System.argv) |> File.read!() |> String.split(" ", trim: true) |> Enum.map(&String.to_integer/1)
|> Enum.map(fn n -> {Day11.c_stone(n, 25), Day11.c_stone(n, 75)} end)
|> Enum.reduce({0,0}, fn {p1, p2}, {acc1, acc2} -> {p1 + acc1, p2 + acc2} end) |> IO.inspect()
