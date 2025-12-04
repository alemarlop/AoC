package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	input := parseInput(os.Args[1])
	p1 := run(slices.Clone(input), func(x int) int { return x + 1 })
	p2 := run(slices.Clone(input), func(x int) int {
		if x >= 3 {
			return x - 1
		} else {
			return x + 1
		}
	})
	fmt.Println(p1, p2)
}

func run(slice []int, calculateNext func(int) int) int {
	var index int
	var steps int

	for index < len(slice) {
		currentElement := slice[index]
		slice[index] = calculateNext(slice[index])
		index += currentElement
		steps++
	}

	return steps
}

func parseInput(path string) []int {
	bytes, _ := os.ReadFile(path)

	var input []int

	for _, line := range strings.Split(string(bytes), "\n") {
		val, _ := strconv.Atoi(line)
		input = append(input, val)
	}

	return input
}
