#!/bin/bash

BENCH_RESULT=$(cargo run -r -- bench | tail -1)
OLD_MSG=$(git log --format=%B -n1)
git commit --amend -m "$OLD_MSG" -m "Bench $BENCH_RESULT"
