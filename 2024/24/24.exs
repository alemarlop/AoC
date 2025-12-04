defmodule Day24 do

  def run(bits, gates, op) do
    populate_missing(bits, gates)
    |> Map.to_list() |> Enum.filter(fn {k, _} -> String.starts_with?(k, op) end) |> Enum.sort_by(fn {k, _} -> k end, :desc)
    |> Enum.map(fn {_k, v} -> v end) |> Enum.join("") |> String.to_integer(2)
  end

  def populate_missing(bits, []), do: bits
  def populate_missing(bits, gates) do
    new_bits = Enum.reduce(gates, bits, fn {{o1, op, o2}, to}, acc_bits ->
       result = solve(Map.get(bits, o1), op, Map.get(bits, o2))
       Map.put(acc_bits, to, result)
    end)

    case new_bits == bits do
      true -> bits
      false -> populate_missing(new_bits, gates)
    end
  end

  def solve(o1, "AND", o2), do: o1 == 1 && o2 == 1 && 1 || 0
  def solve(o1, "OR", o2), do: max(o1, o2)
  def solve(o1, "XOR", o2), do: o1 != o2 && 1 || 0

  def find_swapped_gates(gates) do
    Enum.reduce(gates, [], fn {_, out} = elem, acc ->
      res = xors_must_contain_xyz(elem) || zs_should_come_from_xor(elem) || or_inputs_come_from_and(elem, gates) || and_should_be_xy_or_xor(elem, gates)
      cond do
        is_list(res) -> res ++ acc
        res -> [out | acc]
        true -> acc
      end
    end) |> Enum.sort() |> Enum.uniq() |> Enum.join(",")
  end

  defp xors_must_contain_xyz({{op1, "XOR", op2}, out}) do
    not (String.starts_with?(out, "z") || String.starts_with?(op1, "x") || String.starts_with?(op2, "y") || String.starts_with?(op2, "x") || String.starts_with?(op1, "y"))
  end
  defp xors_must_contain_xyz(_), do: false

  defp zs_should_come_from_xor({{_, op, _}, out}) do
    out != "z45" and String.starts_with?(out, "z") && op != "XOR"
  end

  defp or_inputs_come_from_and({{op1, "OR", op2}, _}, gates) do
    o1 = Enum.any?(gates, fn {{_, op, _}, out} -> op1 == out and op == "AND" end)
    o2 = Enum.any?(gates, fn {{_, op, _}, out} -> op2 == out and op == "AND" end)

    [not o1 && op1, not o2 && op2] |> Enum.filter(& &1)
  end
  defp or_inputs_come_from_and(_, _), do: false

  defp and_should_be_xy_or_xor({{op1, "AND", op2}, _}, gates) do
    o1 = Enum.any?(gates, fn {{_, op, _}, out} -> op1 == out and op == "XOR" end)
    o2 = Enum.any?(gates, fn {{_, op, _}, out} -> op2 == out and op == "XOR" end)
    cond1 = o1 or o2
    cond2 = (String.starts_with?(op1, "x") || String.starts_with?(op1, "y")) && (String.starts_with?(op2, "x") || String.starts_with?(op2, "y"))

    case cond1 or cond2 do
      true -> false
      false -> [o1 && op2 || op1]
    end
  end
  defp and_should_be_xy_or_xor(_, _), do: false
end

[bits, gates] = hd(System.argv()) |> File.read!() |> String.split("\n\n", trim: true)
parsed_bits = String.split(bits, "\n", trim: true) |> Enum.map(& String.split(&1, ": ", trim: true) |> List.to_tuple)
parsed_bits = Enum.map(parsed_bits, fn {k, v} -> {k, String.to_integer(v)} end) |> Map.new()
parsed_gates = String.split(gates, "\n", trim: true) |> Enum.map(& String.split(&1, " ", trim: true))
parsed_gates = Enum.map(parsed_gates, fn [o1, op, o2, _, to] -> {{o1, op, o2}, to} end)

p1 = Day24.run(parsed_bits, parsed_gates, "z")
p2 = Day24.find_swapped_gates(parsed_gates)

IO.inspect({p1, p2})
