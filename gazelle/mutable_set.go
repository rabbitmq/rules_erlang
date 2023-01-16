package erlang

import (
	"sort"
)

type MutableSet[T comparable] map[T]bool

func NewMutableSet[T comparable](items ...T) MutableSet[T] {
	s := make(MutableSet[T])
	s.Add(items...)
	return s
}

func (s MutableSet[T]) IsEmpty() bool {
	return len(s) == 0
}

func (s MutableSet[T]) Contains(item T) bool {
	return s[item]
}

func (s MutableSet[T]) Add(items ...T) {
	for _, item := range items {
		s[item] = true
	}
}

func (s MutableSet[T]) Any() T {
	var result T
	for item, present := range s {
		if present {
			result = item
			break
		}
	}
	return result
}

func (s MutableSet[T]) Subtract(other MutableSet[T]) {
	for item, present := range other {
		if present {
			delete(s, item)
		}
	}
}

func Union[T comparable](sets ...MutableSet[T]) MutableSet[T] {
	result := NewMutableSet[T]()
	for _, set := range sets {
		for item := range set {
			result.Add(item)
		}
	}
	return result
}

func Copy[T comparable](set MutableSet[T]) MutableSet[T] {
	return Union(set, NewMutableSet[T]())
}

func (s MutableSet[T]) Values(compare func(i T, j T) int) []T {
	var values []T
	for item := range s {
		values = append(values, item)
	}
	sort.SliceStable(values, func(i, j int) bool {
		return compare(values[i], values[j]) == -1
	})
	return values
}
