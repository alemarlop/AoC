package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	memory, fns := parseInput(os.Args[1])
	var max int
	for _, fn := range fns {
		partialRes := fn(memory)
		if partialRes > max {
			max = partialRes
		}
	}
	var memoryValues []int
	for _, v := range memory {
		memoryValues = append(memoryValues, v)
	}

	fmt.Println(slices.Max(memoryValues), max)
}

func parseInput(path string) (map[string]int, []func(map[string]int) int) {
	bytes, _ := os.ReadFile(path)
	memory := make(map[string]int)

	var funcs []func(map[string]int) int

	for _, line := range strings.Split(string(bytes), "\n") {
		splittedLine := strings.Split(line, " ")
		elem := splittedLine[0]
		op := splittedLine[1]
		opCons, _ := strconv.Atoi(splittedLine[2])
		conditionVar := splittedLine[4]
		conditionOp := splittedLine[5]
		conditionCons, _ := strconv.Atoi(splittedLine[6])

		memory[elem] = 0

		f := func(memo map[string]int) int {
			increment := opCons
			if op == "dec" {
				increment *= -1
			}

			shouldApply := false
			switch conditionOp {
			case ">":
				shouldApply = memo[conditionVar] > conditionCons
			case "<":
				shouldApply = memo[conditionVar] < conditionCons
			case ">=":
				shouldApply = memo[conditionVar] >= conditionCons
			case "<=":
				shouldApply = memo[conditionVar] <= conditionCons
			case "==":
				shouldApply = memo[conditionVar] == conditionCons
			case "!=":
				shouldApply = memo[conditionVar] != conditionCons
			}

			if shouldApply {
				memo[elem] += increment
			}
			return memo[elem]
		}

		funcs = append(funcs, f)
	}
	return memory, funcs
}
