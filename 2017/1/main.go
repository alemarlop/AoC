package main

import (
	"fmt"
	"os"
	"strconv"
)

func main() {
	var input = parseInput("input.txt")
	part1, part2 := run(input)
	fmt.Println(part1, part2)
}

func run(input []int) (int, int) {
	var r1, r2 int = 0, 0
	var length int = len(input)
	var halfway int = length / 2

	for i := 0; i < length; i++ {
		var current int = input[i]
		var n1 int = input[(i+1)%length]
		var n2 int = input[(i+halfway)%length]

		if current == n1 {
			r1 += current
		}

		if current == n2 {
			r2 += current
		}
	}

	return r1, r2
}

func parseInput(path string) []int {
	bytes, _ := os.ReadFile(path)

	var numbers []int

	for _, number := range string(bytes) {
		n, _ := strconv.Atoi(string(number))
		numbers = append(numbers, n)
	}

	return numbers
}
