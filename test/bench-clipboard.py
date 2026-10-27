"""Compare X11 paste transactions against an earlier clipboard helper revision."""

import argparse
import subprocess
import tempfile
import ctypes as c
import importlib.util
import json
from pathlib import Path
import statistics
import time

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--baseline', default='9ea4ae0')
parser.add_argument('--samples', type=int, default=30)
parser.add_argument('--reads', type=int, default=50)
args = parser.parse_args()
if args.samples < 2 or args.reads < 1:
    parser.error('Use at least two samples and one read per sample')
root = Path(__file__).resolve().parents[1]
spec = importlib.util.spec_from_file_location('clipboard_native', root / 'test/test-clipboard-native.py')
test = importlib.util.module_from_spec(spec)
spec.loader.exec_module(test)
baseline = tempfile.TemporaryDirectory()
for name in ('org_glance_clipboard.py', 'org_glance_clipboard_x11.py'):
    (Path(baseline.name) / name).write_bytes(subprocess.check_output(
        ['git', 'show', args.baseline + ':src/view/' + name], cwd=root))
paths = {'baseline': Path(baseline.name) / 'org_glance_clipboard.py', 'current': test.HELPER}
records = {key: [] for key in paths}
test.X11.setUpClass()
reader = None
try:
    with test.patch.dict(test.os.environ, test.X11.environment):
        reader = test.Clipboard('benchmark')
    x, d = reader.x, reader.display
    x.XConvertSelection.argtypes = [c.c_void_p, c.c_ulong, c.c_ulong, c.c_ulong, c.c_ulong, c.c_ulong]
    x.XGetWindowProperty.argtypes = [c.c_void_p, c.c_ulong, c.c_ulong, c.c_long, c.c_long, c.c_int,
                                    c.c_ulong, c.POINTER(c.c_ulong), c.POINTER(c.c_int),
                                    c.POINTER(c.c_ulong), c.POINTER(c.c_ulong), c.POINTER(c.c_void_p)]
    x.XFree.argtypes = [c.c_void_p]
    def read():
        target = reader.atoms['UTF8_STRING']
        x.XConvertSelection(d, reader.atoms['CLIPBOARD'], target, target, reader.window, 0)
        x.XFlush(d)
        event = test.Event()
        x.XNextEvent(d, c.byref(event))
        assert event.type == 31 and event.notification.property == target
        actual, bits, count, after, data = c.c_ulong(), c.c_int(), c.c_ulong(), c.c_ulong(), c.c_void_p()
        assert x.XGetWindowProperty(d, reader.window, target, 0, 1024, 1, target,
                                   c.byref(actual), c.byref(bits), c.byref(count),
                                   c.byref(after), c.byref(data)) == 0
        assert c.string_at(data, count.value) == b'synthetic-benchmark-value'
        x.XFree(data)
    for iteration in range(args.samples):
        for name in (('baseline', 'current') if iteration % 2 else ('current', 'baseline')):
            case = test.X11('test_memory_transfer_and_primary_unchanged')
            test.HELPER = paths[name]
            process = case.start('x11', b'synthetic-benchmark-value', ttl=20, env=case.environment)
            try:
                case.ready(process)
                for _ in range(5):
                    read()
                start = time.perf_counter()
                for _ in range(args.reads):
                    read()
                records[name].append((time.perf_counter() - start) / args.reads * 1000)
            finally:
                case.doCleanups()
finally:
    if reader is not None:
        reader.close()
    test.X11.tearDownClass()
    baseline.cleanup()
result = {name: {'median_ms': round(statistics.median(samples), 4),
                 'p95_ms': round(statistics.quantiles(samples, n=20)[18], 4),
                 'samples': len(samples), 'reads_per_sample': args.reads} for name, samples in records.items()}
result['samples_ms'] = records
print(json.dumps(result, indent=2))
