#!/bin/bash

# Assumes that an optimized build has already been built
BENCH_RESULT=$(cargo pgo optimize run -- bench | tail -1)
OLD_MSG=$(git log --format=%B -n1)
git commit --amend -m "$OLD_MSG" -m "Bench $BENCH_RESULT"
