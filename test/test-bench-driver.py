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

    def test_process_inventory_and_result_drift_are_errors(self):
        first = dict(record(), **{'samples-ms': [2]})
        for later in ([], [dict(first, fingerprint='changed')], [first, first],
                      [first, dict(first, harness='other')]):
            with self.subTest(later=later):
                with self.assertRaises(ValueError):
                    bench.merge_runs([[first], later])


if __name__ == '__main__':
    unittest.main()
