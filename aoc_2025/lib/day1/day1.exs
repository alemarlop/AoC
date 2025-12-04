import Utils

defmodule Day1 do
  @initial_value 50
  @total_values 100

  def run(password_method) do
    get_lines()
    |> Enum.reduce({@initial_value, 0}, password_method)
    |> elem(1)
  end

  def password_method_1(elem, {acc, res}) do
    case run_line(acc, elem) do
      0 -> {0, res + 1}
      acc -> {acc, res}
    end
  end

  def password_method_2({dir, dis} = elem, {acc, res}) do
    complete_rounds = div(dis, @total_values)
    offset = Integer.mod(dis, @total_values)

    res =
      case dir do
        :L when acc - offset <= 0 and acc != 0 -> 1
        :R when acc + offset >= @total_values and acc != @total_values -> 1
        _otherwhise -> 0
      end + complete_rounds + res

    {run_line(acc, elem), res}
  end

  defp get_lines, do: lines_from_args(&format_lines/1)

  defp format_lines("L" <> num), do: {:L, String.to_integer(num)}
  defp format_lines("R" <> num), do: {:R, String.to_integer(num)}

  defp run_line(acc, {:L, num}), do: Integer.mod(acc - num, @total_values)
  defp run_line(acc, {:R, num}), do: Integer.mod(acc + num, @total_values)
end

run_t(fn -> Day1.run(&Day1.password_method_1/2) end)
run_t(fn -> Day1.run(&Day1.password_method_2/2) end)
