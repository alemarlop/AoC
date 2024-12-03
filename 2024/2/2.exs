defmodule Day2 do

  def run do
    input = format_input()

    p1 = input
    |> Enum.map(&valid?/1)
    |> Enum.count(&(&1))

    p2 = input
    |> Enum.map(&get_combinations([], &1))
    |> Enum.map(fn e -> not is_nil(Enum.find(e, fn i -> valid?(i) end)) end)
    |> Enum.count(&(&1))

    {p1, p2}
  end

  def get_combinations(_, []), do: []
  def get_combinations(list, [h | t]),
    do: [list ++ t] ++ get_combinations(list ++ [h], t)

  def valid?([fst, snd | _] = list) when fst < snd,
    do: valid?(list, :increasing)

  def valid?(list),
    do: valid?(list, :decreasing)

  def valid?([fst, snd | t], :increasing) do
    snd - fst <= 3 && snd - fst > 0 && valid?([snd | t], :increasing)
  end

  def valid?([fst, snd | t], :decreasing) do
    fst - snd <= 3 && fst - snd > 0 && valid?([snd | t], :decreasing)
  end

  def valid?([_], _), do: true

  def format_input do
    System.argv()
    |> hd()
    |> File.stream!()
    |> Stream.map(fn line ->
      line
      |> String.trim()
      |> String.split(" ")
      |> Enum.map(&String.to_integer/1)
    end)
  end

end

IO.inspect(Day2.run)
