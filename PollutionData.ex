defmodule PollutionData do
	def importLinesFromCSV(path) do
		File.read!(path) |> String.split("\n") |> length()
	end 	
	
	def convert(line) do
		[date, hour, c1, c2, value] = String.split(line, ",")
		date = String.split(date, "-") |> Enum.reverse |> Enum.map(fn(x) -> elem(Integer.parse(x), 0) end) |> :erlang.list_to_tuple
		coordinates = {c1, c2}
		keys = [:datetime, :location, :pollutionLevel]
		map = for k <- keys, v <- [date, hour, coordinates, value], into: %{}, do: {k,v} end
		map
	end
end
