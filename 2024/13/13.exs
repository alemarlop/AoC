defmodule Day13 do
  def run_machine({{a_x, a_y}, {b_x, b_y}, {prize_x, prize_y}}) do
    # Mejor hacer el algebra a mano que usar librerias. Nx.LinAlg se hace un lio con numeros grandes
    count_a = (prize_x * b_y - b_x * prize_y) / (b_y * a_x - b_x * a_y);
    count_b = (prize_y * a_x - a_y * prize_x) / (b_y * a_x - b_x * a_y);
    verify_range(count_a, count_b) && round(count_a * 3 + count_b) || 0
  end

  def verify_range(times_a, times_b), do: times_a == Float.floor(times_a) && times_b == Float.floor(times_b)
end

machines = hd(System.argv())
|> File.read!()
|> String.split("\n\n")
|> Enum.map(fn machine ->
  [a, b, prize] = machine |> String.split("\n")
  [a_x, a_y] = Regex.scan(~r/(\d+)/, a) |> Enum.map(fn [x, _] -> x end) |> Enum.map(&String.to_integer/1)
  [b_x, b_y] = Regex.scan(~r/(\d+)/, b) |> Enum.map(fn [x, _] -> x end) |> Enum.map(&String.to_integer/1)
  [prize_x, prize_y] = Regex.scan(~r/(\d+)/, prize) |> Enum.map(fn [x, _] -> x end) |> Enum.map(&String.to_integer/1)
  {{a_x, a_y}, {b_x, b_y}, {prize_x, prize_y}, {prize_x + 10000000000000, prize_y + 10000000000000}}
  end)

{
  Enum.reduce(machines, 0, fn {{a_x, a_y}, {b_x, b_y}, {prize_x, prize_y}, _}, acc ->
    Day13.run_machine({{a_x, a_y}, {b_x, b_y}, {prize_x, prize_y}}) + acc end),
  Enum.reduce(machines, 0, fn {{a_x, a_y}, {b_x, b_y}, _, {prize_x, prize_y}}, acc ->
    Day13.run_machine({{a_x, a_y}, {b_x, b_y}, {prize_x, prize_y}}) + acc end)
} |> IO.inspect()
