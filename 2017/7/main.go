package main

import (
	"fmt"
	"os"
	"slices"
	"strconv"
	"strings"
)

func main() {
	tree, weights, keys, values := parseInput(os.Args[1])

	var root string
	for _, key := range keys {
		if !slices.Contains(values, key) {
			root = key
			break
		}
	}

	defer func() {
		if weight := recover(); weight != nil {
			fmt.Println(root, weight)
		}
	}()

	calculateWeight(tree, weights, root)

}

func calculateWeight(tree map[string][]string, weights map[string]int, root string) int {
	currentWeight := weights[root]
	children := tree[root]
	var childrenWeights []int
	var total int
	for _, child := range children {
		w := calculateWeight(tree, weights, child)
		childrenWeights = append(childrenWeights, w)
		total += w
	}

	for affectedIndex, weight := range childrenWeights {
		if weight != childrenWeights[0] {
			diff := weight - childrenWeights[0]
			result := weights[children[affectedIndex]] - diff
			panic(result)
		}
	}

	total += currentWeight
	return total
}

func parseInput(path string) (map[string][]string, map[string]int, []string, []string) {
	bytes, _ := os.ReadFile(path)
	result := make(map[string][]string)
	weights := make(map[string]int)
	var keys []string
	var values []string
	for _, line := range strings.Split(string(bytes), "\n") {
		lr := strings.Split(line, " -> ")
		left := strings.Split(lr[0], " ")[0]
		raw_weight := strings.Split(lr[0], " ")[1]
		weight, _ := strconv.Atoi(raw_weight[1 : len(raw_weight)-1])
		weights[left] = weight
		keys = append(keys, left)
		if len(lr) == 1 {
			result[left] = []string{}
			continue
		}
		right := lr[1]
		result[left] = strings.Split(right, ", ")
		values = slices.Concat(values, result[left])
	}
	return result, weights, keys, values
}
