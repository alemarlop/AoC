defmodule Day22 do
  import Bitwise

  @prune_modulo 16777216

  def run_p1(sec_i, times), do: Enum.map(sec_i, & List.last(run(&1, times))) |> Enum.sum

  def run_p2(sec_i, times) do
    prices_diffs = Enum.map(sec_i, & run(&1, times) |> get_prices_and_variations())

    sequences = Stream.flat_map(prices_diffs, & calculate_vals(MapSet.new(), &1)) |> Enum.map(&Tuple.to_list/1)
    frequencies = Enum.frequencies(sequences)
    max_freq = Enum.max(Map.values(frequencies))
    sequences = Enum.reject(sequences |> Enum.uniq(), & Map.get(frequencies, &1) < max_freq - 40)
    values_for_sequence = Task.async_stream(sequences, fn seq ->
      res = Task.async_stream(prices_diffs, & calculate_value(&1, seq))
      Enum.reduce(res, 0, fn {:ok, elem}, acc -> acc + elem end)
    end)
    Enum.max(values_for_sequence)
  end

  def get_prices_and_variations(secrets) do
    prices = Enum.map(secrets, & rem(&1, 10))
    offset_prices = [nil | Enum.take(prices, length(prices) - 1)]
    zipped = Enum.zip(prices, offset_prices)
    Enum.map(zipped, fn {price, offset} -> {price, offset && price - offset || nil} end)
  end

  def calculate_value([{_, s1}, {_, s2}, {_, s3}, {val, s4} | _], [s1, s2, s3, s4]), do: val
  def calculate_value([_ | t], seq), do: calculate_value(t, seq)
  def calculate_value([], _seq), do: 0

  def calculate_vals(freq, [{_, s1}, {v2, s2}, {v3, s3}, {v4, s4} | t]) do
    calculate_vals(MapSet.put(freq, {s1, s2, s3, s4}), [{v2, s2}, {v3, s3}, {v4, s4} | t])
  end
  def calculate_vals(freq, _), do: freq

  def run(secret, 0), do: [secret]
  def run(secret, times), do: [secret | run(next_secret(secret), times-1)]

  defp next_secret(current_secret) do
    ph1 = mix(current_secret, current_secret * 64) |> prune
    ph2 = mix(ph1, div(ph1,32)) |> prune
    mix(ph2, ph2 * 2048) |> prune
  end

  defp mix(secret, value), do: bxor(secret, value)
  defp prune(secret, modulo \\ @prune_modulo), do: rem(secret, modulo)
end

input = hd(System.argv()) |> File.read!() |> String.split("\n", trim: true) |> Enum.map(&String.to_integer/1)
IO.inspect({Day22.run_p1(input, 2000), Day22.run_p2(input, 2000)})
