# Speed Test Report

**Last Run:** 2026-02-07 18:34:04 CST
**Overall Status:** PASS
**Tolerance Factor:** 4.00x baseline

## Results

| Benchmark | Current | Baseline | Threshold | Ratio | Status |
|-----------|---------|----------|-----------|-------|--------|
| build_prompt | 0.0837s | 0.09s | 0.36s | 0.93x | PASS |
| helper_funcs | 0.0461s | 0.07s | 0.28s | 0.66x | PASS |
| combined | 0.0741s | 0.09s | 0.36s | 0.81x | PASS |
| agent_search | 31.5s | 18s | 70s | 1.79x | PASS |

## Baseline Reference

Baselines were established on Dec 19, 2024. Tests fail if current time exceeds
`baseline * 4.00`.

| Benchmark | Baseline | Operations per Run |
|-----------|----------|-------------------|
| build_prompt | 0.09s | 10 iterations x 50 calls x 3 templates |
| helper_funcs | 0.07s | 10 iterations x 100 calls x 5 functions |
| combined | 0.09s | 10 iterations of mixed workload |
| agent_search | 18s | 1 search task (API + network latency) |

