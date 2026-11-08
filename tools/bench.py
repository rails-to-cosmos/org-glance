#!/usr/bin/env python3
"""Measure the current production code and an optional Git baseline with one driver."""

import argparse
import io
import json
import os
from pathlib import Path
import platform
import random
import shutil
import statistics
import subprocess
import tarfile
import tempfile

ROOT = Path(__file__).resolve().parents[1]


def command(argv, **kwargs):
    result = subprocess.run(argv, cwd=ROOT, text=True, capture_output=True, **kwargs)
    if result.returncode:
        raise RuntimeError(f"{' '.join(map(str, argv))}\n{result.stdout}\n{result.stderr}")
    return result.stdout


def prepare(destination, revision=None):
    if revision:
        archive = subprocess.check_output(
            ['git', 'archive', revision, 'org-glance.el', 'src'], cwd=ROOT)
        with tarfile.open(fileobj=io.BytesIO(archive)) as tree:
            for member in tree.getmembers():
                target = destination / member.name
                if member.isfile() and target.suffix in ('.el', '.py'):
                    target.resolve().relative_to(destination.resolve())
                    target.parent.mkdir(parents=True, exist_ok=True)
                    target.write_bytes(tree.extractfile(member).read())
    else:
        for directory in (ROOT, ROOT / 'src/data', ROOT / 'src/view'):
            for source in directory.iterdir():
                if source.is_file() and source.suffix in ('.el', '.py'):
                    target = destination / source.relative_to(ROOT)
                    target.parent.mkdir(parents=True, exist_ok=True)
                    shutil.copyfile(source, target)
    test = destination / 'test'
    test.mkdir(exist_ok=True)
    for source in [*ROOT.glob('test/bench*.el'), ROOT / 'test/test-helpers.el']:
        shutil.copyfile(source, test / source.name)
    paths = [destination, destination / 'src/data', destination / 'src/view', test]
    emacs = ['eask', 'emacs', '--batch']
    for path in paths:
        emacs += ['-L', str(path)]
    sources = [destination / 'org-glance.el',
               *sorted((destination / 'src/data').glob('*.el')),
               *sorted((destination / 'src/view').glob('*.el'))]
    command(emacs + ['--eval', '(setq load-prefer-newer t)', '-f', 'batch-byte-compile',
                     *map(str, sources)])
    return emacs


def measure(emacs, name, directory, environment):
    source = Path(emacs[emacs.index('-L') + 1])
    admission = ('(dolist (function (quote (org-glance-graph:get-headline '
                 'org-glance-cache:headline))) (unless (file-in-directory-p '
                 '(symbol-file function (quote defun)) ' + json.dumps(str(source)) +
                 ') (error "Benchmark loaded libraries outside the selected revision")))')
    expression = ('(org-glance-bench:dispatch ' + json.dumps(name) + ' ' +
                  (json.dumps(str(directory)) if directory else 'nil') + ')')
    output = command(emacs + ['-l', 'bench-runner', '--eval', admission, '--eval', expression], env=environment)
    records = [json.loads(line.removeprefix('OG_BENCH_JSON '))
               for line in output.splitlines() if line.startswith('OG_BENCH_JSON ')]
    if not records:
        raise RuntimeError('Benchmark emitted no measurement records\n' + output)
    return records


def merge_runs(runs):
    merged = {}
    expected = None
    for records in runs:
        seen = set()
        for record in records:
            key = record['harness'], record['operation'], record['size']
            if key in seen:
                raise ValueError('Benchmark emitted duplicate workload identities')
            seen.add(key)
            if key not in merged:
                merged[key] = dict(record, **{'samples-ms': [], 'work': [],
                                             'gc-counts': [], 'gc-ms': [],
                                             'process-samples-ms': []})
            target = merged[key]
            if target.get('fingerprint') != record.get('fingerprint'):
                raise ValueError(f'Result changed between processes for {key}')
            if not record.get('samples-ms'):
                raise ValueError(f'Workload emitted no latency samples for {key}')
            target['process-samples-ms'].append(list(record['samples-ms']))
            for field in ('samples-ms', 'work', 'gc-counts', 'gc-ms'):
                target[field].extend(record.get(field, []))
        if expected is None:
            expected = seen
        elif seen != expected:
            raise ValueError('Workload inventory changed between processes')
    for record in merged.values():
        record['median-ms'] = statistics.median(record['samples-ms'])
    return list(merged.values())


def paired_uncertainty(baseline, current):
    previous = baseline.get('process-samples-ms', [])
    measured = current.get('process-samples-ms', [])
    if len(previous) != len(measured):
        raise ValueError('Baseline and current process-pair counts differ')
    deltas = [statistics.median(after) - statistics.median(before)
              for before, after in zip(previous, measured)]
    interval = None
    if len(deltas) >= 3:
        rng = random.Random(0)
        draws = sorted(statistics.mean(rng.choices(deltas, k=len(deltas)))
                       for _ in range(10000))
        interval = [draws[249], draws[9749]]
    return {'process_pairs': len(deltas),
            'paired_delta_ms': statistics.mean(deltas) if deltas else None,
            'paired_delta_ci95_ms': interval}


def compare(baseline, current):
    def key(record):
        return record['harness'], record['operation'], record['size']
    previous = {key(record): record for record in baseline}
    current_keys = {key(record) for record in current}
    if len(previous) != len(baseline) or len(current_keys) != len(current):
        raise ValueError('Benchmark emitted duplicate workload identities')
    if current_keys != set(previous):
        raise ValueError('Baseline and current workload inventories differ')
    result = []
    for record in current:
        old = previous[key(record)]
        result.append(dict(paired_uncertainty(old, record), **{
                       'harness': record['harness'], 'operation': record['operation'],
                       'size': record['size'], 'baseline_ms': old['median-ms'],
                       'current_ms': record['median-ms'],
                       'ratio': (record['median-ms'] / old['median-ms']
                                 if old['median-ms'] else None),
                       'fingerprints_equal': (old['fingerprint'] == record['fingerprint']
                                              if old.get('fingerprint') and record.get('fingerprint')
                                              else None)}))
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('name', nargs='?', default='all')
    parser.add_argument('--baseline', help='Git commit whose production libraries are measured')
    parser.add_argument('--directory', type=Path, help='Existing cache for a current-only run')
    tier = parser.add_mutually_exclusive_group()
    tier.add_argument('--quick', action='store_true')
    tier.add_argument('--large', action='store_true')
    parser.add_argument('--samples', type=int, default=7)
    parser.add_argument('--pairs', type=int,
                        help='Alternating process pairs (default: 3 with a baseline, 1 otherwise)')
    parser.add_argument('--sizes', help='Positive comma-separated synthetic store sizes')
    args = parser.parse_args()
    if args.pairs is None:
        args.pairs = 3 if args.baseline else 1
    if args.directory:
        args.directory = args.directory.expanduser().resolve()
    if args.samples < 1 or args.pairs < 1:
        parser.error('--samples and --pairs must be positive')
    if args.baseline and args.directory:
        parser.error('Commit comparisons use disposable stores; omit --directory')
    environment = dict(os.environ, OG_BENCH_SAMPLES=str(args.samples))
    if args.quick or args.large:
        environment.pop('OG_BENCH_QUICK', None)
        environment.pop('OG_BENCH_LARGE', None)
        environment['OG_BENCH_QUICK' if args.quick else 'OG_BENCH_LARGE'] = '1'
    if args.sizes:
        environment['OG_BENCH_SIZES'] = args.sizes
    current = command(['git', 'rev-parse', 'HEAD']).strip()
    baseline = (command(['git', 'rev-parse', '--verify', args.baseline + '^{commit}']).strip()
                if args.baseline else None)
    result = {'format': 1, 'current_revision': current, 'baseline_revision': baseline,
              'dirty': bool(command(['git', 'status', '--porcelain'])),
              'platform': platform.platform(), 'process_pairs': args.pairs, 'runs': {}}
    with tempfile.TemporaryDirectory(prefix='org-glance-bench-') as temporary:
        prepared = {}
        for label, revision in [('baseline', baseline), ('current', None)]:
            if label == 'baseline' and baseline is None:
                continue
            destination = Path(temporary) / label
            destination.mkdir()
            prepared[label] = prepare(destination, revision)
        collected = {label: [] for label in prepared}
        for iteration in range(args.pairs):
            labels = list(prepared)
            if iteration % 2:
                labels.reverse()
            for label in labels:
                collected[label].append(measure(prepared[label], args.name,
                                                args.directory, environment))
        result['runs'] = {label: merge_runs(runs) for label, runs in collected.items()}

    if baseline:
        result['comparison'] = compare(result['runs']['baseline'], result['runs']['current'])
    print(json.dumps(result, indent=2))


if __name__ == '__main__':
    main()
