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
	fmt.Println(run(input), run(input))
}

func run(input []int) int {
	var visited []string
	var steps int

	for {
		currentState := fmt.Sprint(input)
		if slices.Contains(visited, currentState) {
			break
		}
		steps++
		visited = append(visited, fmt.Sprint(input))
		max := slices.Max(input)
		index := slices.Index(input, max)
		input[index] = 0
		reallocate(input, index, max)
	}

	return steps
}

func reallocate(input []int, index int, value int) {
	for i := 1; i <= value; i++ {
		newIndex := (index + i) % len(input)
		input[newIndex]++
	}
}

func parseInput(path string) []int {
	bytes, _ := os.ReadFile(path)
	var result []int
	for _, num := range strings.Split(string(bytes), "\t") {
		parsedNum, _ := strconv.Atoi(num)
		result = append(result, parsedNum)
	}
	return result
}
