package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	var input [][]int = parseInput("input.txt")
	fmt.Println(part1(input), part2(input))
}

func part1(input [][]int) int {
	var result int
	for _, line := range input {
		result += slices.Max(line) - slices.Min(line)
	}
	return result
}

func part2(input [][]int) int {
	var result int

	for _, line := range input {
		result += calculateForLine(line)
	}

	return result
}

func calculateForLine(line []int) int {
	for _, d1 := range line {
		for _, d2 := range line {
			if d1 != d2 && d2 != 0 && d1%d2 == 0 {
				return d1 / d2
			}
		}
	}
	return 0
}

func parseInput(path string) [][]int {
	bytes, _ := os.ReadFile(path)
	var result [][]int
	var lines []string = strings.Split(string(bytes), "\n")

	for _, line := range lines {
		var parsedLine []int
		for _, elem := range strings.Split(line, "\t") {
			val, _ := strconv.Atoi(string(elem))
			parsedLine = append(parsedLine, val)
		}
		result = append(result, parsedLine)
	}

	return result
}
