import Utils

defmodule Day3 do
  def run(len) do
    lines_from_args(&parse_line/1)
    |> Enum.map(&get_num_from_line(&1, len))
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
  end

  defp parse_line(line) do
    line
    |> String.split("", trim: true)
    |> Enum.map(&String.to_integer/1)
    |> Enum.with_index()
  end

  defp get_num_from_line(_num_list, 0), do: ""

  defp get_num_from_line(num_list, len) do
    {max_val, max_i} =
      num_list
      |> Enum.slice(0..(length(num_list) - len))
      |> Enum.max_by(fn {val, _i} -> val end)

    new_num_list =
      num_list
      |> Enum.map(fn {val, _i} -> val end)
      |> Enum.slice((max_i + 1)..length(num_list))
      |> Enum.with_index()

    "#{max_val}" <> get_num_from_line(new_num_list, len - 1)
  end
end

run_t(fn -> Day3.run(2) end)
run_t(fn -> Day3.run(12) end)
