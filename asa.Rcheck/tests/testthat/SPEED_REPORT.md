# Speed Test Report

**Last Run:** 2025-12-19 22:30:33 EST
**Overall Status:** PASS
**Tolerance Factor:** 1.25x baseline

## Results

| Benchmark | Current | Baseline | Threshold | Ratio | Status |
|-----------|---------|----------|-----------|-------|--------|
| build_prompt | 0.0828s | 0.09s | 0.11s | 0.92x | PASS |
| helper_funcs | 0.0692s | 0.07s | 0.09s | 0.99x | PASS |
| combined | 0.0957s | 0.09s | 0.11s | 1.05x | PASS |
| agent_search | 12.7s | 18s | 22s | 0.72x | PASS |

## Baseline Reference

Baselines were established on Dec 19, 2024. Tests fail if current time exceeds
`baseline * 1.25`.

| Benchmark | Baseline | Operations per Run |
|-----------|----------|-------------------|
| build_prompt | 0.09s | 10 iterations x 50 calls x 3 templates |
| helper_funcs | 0.07s | 10 iterations x 100 calls x 5 functions |
| combined | 0.09s | 10 iterations of mixed workload |
| agent_search | 18s | 1 search task (API + network latency) |

