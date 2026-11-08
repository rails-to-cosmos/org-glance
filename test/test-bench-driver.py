"""Contracts for matching measurements from two real production revisions."""

import importlib.util
from pathlib import Path
import unittest
import sys

sys.dont_write_bytecode = True

spec = importlib.util.spec_from_file_location('bench', Path(__file__).resolve().parents[1] / 'tools/bench.py')
bench = importlib.util.module_from_spec(spec)
spec.loader.exec_module(bench)


def record(name='readers', elapsed=2, fingerprint='same'):
    return {'harness': name, 'operation': 'point', 'size': 10,
            'median-ms': elapsed, 'fingerprint': fingerprint}


class Comparison(unittest.TestCase):
    def test_compare_real_results_without_replaying_an_old_algorithm(self):
        result = bench.compare([record()], [record(elapsed=1, fingerprint='new')])[0]
        self.assertEqual(result['ratio'], .5)
        self.assertFalse(result['fingerprints_equal'])

    def test_missing_or_duplicated_workloads_are_errors(self):
        for previous, current in [([record()], []), ([], [record()]),
                                  ([record(), record()], [record()]),
                                  ([record()], [record(), record()])]:
            with self.subTest(previous=previous, current=current):
                with self.assertRaises(ValueError):
                    bench.compare(previous, current)

    def test_zero_duration_does_not_invent_a_ratio(self):
        self.assertIsNone(bench.compare([record(elapsed=0)], [record()])[0]['ratio'])


class RepeatedProcesses(unittest.TestCase):
    def test_every_process_contributes_samples(self):
        first = dict(record(), **{'samples-ms': [2], 'work': [{'opens': 0}]})
        second = dict(record(elapsed=4), **{'samples-ms': [4], 'work': [{'opens': 1}]})
        merged = bench.merge_runs([[first], [second]])[0]
        self.assertEqual(merged['median-ms'], 3)
        self.assertEqual(merged['samples-ms'], [2, 4])
        self.assertEqual(merged['work'], [{'opens': 0}, {'opens': 1}])
        self.assertEqual(merged['process-samples-ms'], [[2], [4]])

    def test_process_inventory_and_result_drift_are_errors(self):
        first = dict(record(), **{'samples-ms': [2]})
        for later in ([], [dict(first, fingerprint='changed')], [first, first],
                      [first, dict(first, harness='other')]):
            with self.subTest(later=later):
                with self.assertRaises(ValueError):
                    bench.merge_runs([[first], later])

    def test_uncertainty_uses_independent_process_pairs(self):
        previous = dict(record(), **{'process-samples-ms': [[2, 4], [4, 6], [6, 8]]})
        current = dict(record(elapsed=4), **{'process-samples-ms': [[4, 6], [6, 8], [8, 10]]})
        result = bench.compare([previous], [current])[0]
        self.assertEqual(result['process_pairs'], 3)
        self.assertEqual(result['paired_delta_ms'], 2)
        self.assertEqual(result['paired_delta_ci95_ms'], [2, 2])

    def test_single_process_has_no_between_process_interval(self):
        previous = dict(record(), **{'process-samples-ms': [[2] * 100]})
        current = dict(record(elapsed=4), **{'process-samples-ms': [[4] * 100]})
        result = bench.compare([previous], [current])[0]
        self.assertEqual(result['process_pairs'], 1)
        self.assertIsNone(result['paired_delta_ci95_ms'])
        current['process-samples-ms'].append([4])
        with self.assertRaises(ValueError):
            bench.compare([previous], [current])

    def test_interval_retains_process_variation(self):
        previous = dict(record(elapsed=10), **{'process-samples-ms': [[10], [10], [10]]})
        current = dict(record(elapsed=10), **{'process-samples-ms': [[2], [10], [18]]})
        result = bench.compare([previous], [current])[0]
        self.assertEqual(result['paired_delta_ms'], 0)
        lower, upper = result['paired_delta_ci95_ms']
        self.assertLess(lower, 0)
        self.assertGreater(upper, 0)
        self.assertEqual(result, bench.compare([previous], [current])[0])

    def test_empty_process_sample_is_rejected(self):
        with self.assertRaises(ValueError):
            bench.merge_runs([[dict(record(), **{'samples-ms': []})]])


if __name__ == '__main__':
    unittest.main()
