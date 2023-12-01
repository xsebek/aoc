#!/bin/bash

set +x

YEAR=$(date '+%Y')
DAY=$(date '+%d')

YEAR="${1:-$YEAR}"
DAY="${2:-$DAY}"

cp "A${YEAR}/DayDAY.hs" "A${YEAR}/Day${DAY}.hs"

sed -i "s/DAY/${DAY}/g" "A${YEAR}/Day${DAY}.hs"

sed -i \
  "s/-- import Day${DAY}/import Day${DAY}/g; s/todoDay -- main${DAY}/main${DAY}/g" \
  "app/${YEAR}/Main.hs"

sed -i "s/-- Day${DAY}/Day${DAY}/g" 'aoc.cabal'
