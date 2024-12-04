run = fn filter_expression ->
  contents = hd(System.argv) |> File.read!() |> String.replace(filter_expression, "")
  Regex.scan(~r/(?<=mul\()\d{1,3}\,\d{1,3}(?=\))/, contents)
  |> Enum.map(fn [match] -> match |> String.split(",") |> Enum.map(&String.to_integer/1) end)
  |> Enum.reduce(0, fn [a, b], acc -> acc + a * b end)
end
IO.inspect({run.(~r/^$/), run.(~r/(don't\(\).*?do\(\))/s)})
