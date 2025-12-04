package main

import (
	"fmt"
	"os"
	"slices"
	"strings"
)

func main() {
	input := readInput(os.Args[1])
	fmt.Println(run(input))
}

func run(input [][]string) (int, int) {
	var r1 int
	var r2 int
	for _, line := range input {
		if len(line) == len(buildSet(line)) {
			r1++
		}

		var orderedWords []string
		for _, word := range line {
			chars := []rune(word)
			slices.Sort(chars)
			orderedWords = append(orderedWords, string(chars))
		}

		if len(line) == len(buildSet(orderedWords)) {
			r2++
		}
	}
	return r1, r2
}

func buildSet(slice []string) map[string]struct{} {
	set := make(map[string]struct{})
	for _, word := range slice {
		set[word] = struct{}{}
	}
	return set
}

func readInput(path string) [][]string {
	contents, _ := os.ReadFile(path)
	var result [][]string

	for _, line := range strings.Split(string(contents), "\n") {
		result = append(result, strings.Split(line, " "))
	}

	return result
}
