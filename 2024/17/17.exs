defmodule Day17 do
  def run_opcode(opcode, combo, {a, b, c, out, pointer}, target) do
    case opcode do
      0 -> {trunc(a / :math.pow(2, get_combo(combo, {a, b, c}))), b, c, out, pointer + 2}
      1 -> {a, Bitwise.bxor(b, combo), c, out, pointer + 2}
      2 -> {a, Integer.mod(get_combo(combo, {a, b, c}), 8), c, out, pointer + 2}
      3 when a == 0 -> {0, b, c, out, pointer + 2}
      3 -> {a, b, c, out, combo}
      4 -> {a, Bitwise.bxor(b, c), c, out, pointer + 2}
      5 ->
        n_out_value = Integer.mod(get_combo(combo, {a, b, c}), 8)
        case target do
          nil -> {a, b, c, [n_out_value | out], pointer + 2}
          _ -> Enum.at(target, length(out)) == n_out_value && {a, b, c, [n_out_value | out], pointer + 2} || raise "Error"
        end

      6 -> {a, trunc(a / :math.pow(2, get_combo(combo, {a, b, c}))), c, out, pointer + 2}
      7 -> {a, b, trunc(a / :math.pow(2, get_combo(combo, {a, b, c}))), out, pointer + 2}
    end
  end

  def get_combo(value, _) when value <= 3, do: value
  def get_combo(4, {a, _, _}), do: a
  def get_combo(5, {_, b, _}), do: b
  def get_combo(6, {_, _, c}), do: c

  def run_program({a, b, c, out, pointer}, sequence, target) do
    case pointer > length(sequence) - 1 do
      true -> Enum.reverse(out)
      false ->
        {opcode, operand} = {Enum.at(sequence, pointer), Enum.at(sequence, pointer + 1)}
        run_program(run_opcode(opcode, operand, {a, b, c, out, pointer}, target), sequence, target)
    end
  end

  def find_new_value({a, b, c, out, pointer}, sequence, target) do
    if rem(a, 1_000_000) == 0, do: IO.inspect(a)
    res = run_program({a, b, c, out, pointer}, sequence, sequence)
    case res == target do
      true -> a
      false -> find_new_value({a + 1, b, c, out, pointer}, sequence, target)
    end
  rescue
    _ ->
      find_new_value({a + 1, b, c, out, pointer}, sequence, target)
  end

  def find_target(target, a) do
    result = run_program({a, 0, 0, [], 0}, target, nil)
    target_s = Enum.join(target, ",")
    result_s = Enum.join(result, ",")
    cond do
      result == target -> a
      String.ends_with?(target_s, result_s) -> find_target(target, a * 8)
      true -> find_target(target, a + 1)
    end
  end

end

{
  Day17.run_program({41644071, 0, 0, [], 0}, [2,4,1,2,7,5,1,7,4,4,0,3,5,5,3,0], nil) |> Enum.join(","),
  Day17.find_target([2,4,1,2,7,5,1,7,4,4,0,3,5,5,3,0], 0)
} |> IO.inspect()
