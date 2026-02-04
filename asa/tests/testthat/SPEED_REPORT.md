# Speed Test Report

**Last Run:** 2026-02-03 20:02:36 CST
**Overall Status:** FAIL
**Tolerance Factor:** 4.00x baseline

## Results

| Benchmark | Current | Baseline | Threshold | Ratio | Status |
|-----------|---------|----------|-----------|-------|--------|
| build_prompt | 0.0877s | 0.09s | 0.36s | 0.97x | PASS |
| helper_funcs | 0.0505s | 0.07s | 0.28s | 0.72x | PASS |
| combined | 0.0773s | 0.09s | 0.36s | 0.85x | PASS |
| agent_search | 73.4s | 18s | 70s | 4.17x | FAIL |

## Baseline Reference

Baselines were established on Dec 19, 2024. Tests fail if current time exceeds
`baseline * 4.00`.

| Benchmark | Baseline | Operations per Run |
|-----------|----------|-------------------|
| build_prompt | 0.09s | 10 iterations x 50 calls x 3 templates |
| helper_funcs | 0.07s | 10 iterations x 100 calls x 5 functions |
| combined | 0.09s | 10 iterations of mixed workload |
| agent_search | 18s | 1 search task (API + network latency) |

