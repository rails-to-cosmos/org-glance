"""Exercise helper ownership, anonymous storage and deadlines across processes."""

from ctypes import CDLL, byref, c_size_t, c_void_p, string_at
import os
from pathlib import Path
import resource
import re
import select
import shutil
import signal
import socket
import struct
import subprocess
import sys
import tempfile
import threading
import time
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
HELPER = ROOT / 'src/view/org_glance_clipboard.py'
sys.path.insert(0, str(HELPER.parent))
from org_glance_clipboard import Deadline, Secret, send_fd
from org_glance_clipboard_wayland import string, words, read_string
from org_glance_clipboard_x11 import Clipboard, Event, Request


class Compositor:
    """Model data-control ownership over a real Wayland socket and transfer fds."""
    def __init__(self, directory, protocol='ext_data_control_manager_v1', echo=True):
        self.path = str(Path(directory) / 'wayland-test')
        self.server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.server.bind(self.path)
        self.server.listen(1)
        self.protocol, self.echo = protocol, echo
        self.connection = None
        self.lock = threading.Lock()
        self.selected = threading.Event()
        self.closed = threading.Event()
        self.objects = {1: 'display'}
        self.offers = []
        self.selection_requests = []
        self.source = self.device = None
        self.thread = threading.Thread(target=self.run, daemon=True)
        self.thread.start()

    def event(self, obj, opcode, data=b'', fd=None):
        packet = words(obj, ((len(data) + 8) << 16) | opcode) + data
        with self.lock:
            if fd is None:
                self.connection.sendall(packet)
            else:
                self.connection.sendmsg([packet], [(socket.SOL_SOCKET, socket.SCM_RIGHTS,
                                                    struct.pack('i', fd))])

    def run(self):
        try:
            self.connection, _ = self.server.accept()
            buffer = b''
            while True:
                chunk = self.connection.recv(65536)
                if not chunk:
                    return
                buffer += chunk
                while len(buffer) >= 8:
                    obj, header = struct.unpack_from('=II', buffer)
                    size, opcode = header >> 16, header & 65535
                    if len(buffer) < size:
                        break
                    data, buffer = buffer[8:size], buffer[size:]
                    kind = self.objects.get(obj)
                    if kind == 'display' and opcode == 1:
                        registry, = struct.unpack('=I', data)
                        self.objects[registry] = 'registry'
                        self.event(registry, 0, words(1) + string('wl_seat') + words(1))
                        if self.protocol:
                            self.event(registry, 0, words(2) + string(self.protocol) + words(1))
                    elif kind == 'display' and opcode == 0:
                        callback, = struct.unpack('=I', data)
                        self.event(callback, 0, words(1))
                        self.event(1, 1, words(callback))
                    elif kind == 'registry':
                        name, offset = read_string(data, 4)
                        _, new_id = struct.unpack_from('=II', data, offset)
                        self.objects[new_id] = 'manager' if 'control' in name else 'seat'
                    elif kind == 'manager' and opcode == 1:
                        self.device, _ = struct.unpack('=II', data)
                        self.objects[self.device] = 'device'
                        self.event(self.device, 1, words(0))
                    elif kind == 'manager' and opcode == 0:
                        self.source, = struct.unpack('=I', data)
                        self.objects[self.source] = 'source'
                    elif kind == 'source' and opcode == 0:
                        self.offers.append(read_string(data)[0])
                    elif kind == 'device' and opcode == 0:
                        self.selection_requests.append(struct.unpack('=I', data)[0])
                        if self.echo:
                            offer = 0xff000001
                            self.event(self.device, 0, words(offer))
                            for mime in self.offers:
                                self.event(offer, 0, string(mime))
                            self.event(self.device, 1, words(offer))
                        self.selected.set()
        except (OSError, EOFError):
            pass
        finally:
            self.closed.set()

    def request(self, mime='text/plain;charset=utf-8'):
        read, write = os.pipe()
        try:
            self.event(self.source, 0, string(mime), write)
        finally:
            os.close(write)
        return read

    def close(self):
        if self.connection:
            try:
                self.connection.shutdown(socket.SHUT_RDWR)
            except OSError:
                pass
            self.connection.close()
        self.server.close()
        self.thread.join(timeout=2)


class Native(unittest.TestCase):
    def start(self, backend, payload, ttl=1, env=None, preexec=None, audit=False):
        command = [sys.executable, '-I', '-B']
        if audit:
            command += ['-c', "import os,runpy,sys; "
                        "sys.addaudithook(lambda event,args: "
                        "(_ for _ in ()).throw(RuntimeError('file write')) "
                        "if event == 'open' and args[2] & (os.O_WRONLY|os.O_RDWR|os.O_CREAT) else None); "
                        "sys.argv=sys.argv[1:]; runpy.run_path(sys.argv[0],run_name='__main__')"]
        command += [str(HELPER), backend, str(len(payload)), str(ttl), 'fixture']
        process = subprocess.Popen(command, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                   stderr=subprocess.DEVNULL, env=env, preexec_fn=preexec)
        self.addCleanup(self.stop, process)
        process.stdin.write(payload)
        process.stdin.close()
        return process

    @staticmethod
    def stop(process):
        if process.poll() is None:
            process.send_signal(signal.SIGCONT)
            process.terminate()
        process.wait(timeout=3)
        if process.stdout:
            process.stdout.close()

    def ready(self, process):
        self.assertTrue(select.select([process.stdout], [], [], 3)[0])
        self.assertEqual(process.stdout.readline(), b'ready\n')



class Memory(Native):
    def test_suspend_clock_jump_expires(self):
        now = [10.0]
        deadline = Deadline(15, clock=lambda: now[0])
        now[0] += 3600
        self.assertEqual(deadline.remaining(), 0)

    def test_regular_file_transfer_refused(self):
        with tempfile.TemporaryFile() as file:
            send_fd(os.dup(file.fileno()), b'credential', Deadline(10))
            self.assertEqual(file.read(), b'')


class Wayland(Native):
    def compositor(self, **options):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        server = Compositor(directory.name, **options)
        self.addCleanup(server.close)
        return server, dict(os.environ, WAYLAND_DISPLAY=server.path)

    def test_memory_transfer_and_hint(self):
        for protocol in ('ext_data_control_manager_v1', 'zwlr_data_control_manager_v1'):
            with self.subTest(protocol=protocol):
                server, env = self.compositor(protocol=protocol)
                value = 'credential λ\nsecond line'.encode()
                process = self.start('wayland', value, env=env, audit=True)
                self.ready(process)
                for mime, expected in [('text/plain', value), ('x-kde-passwordManagerHint', b'secret')]:
                    fd = server.request(mime)
                    with os.fdopen(fd, 'rb') as pipe:
                        self.assertEqual(pipe.read(), expected)
                process.wait(timeout=3)
                self.assertTrue(server.closed.wait(1))

    def test_no_publication_echo_means_no_ack(self):
        server, env = self.compositor(echo=False)
        process = self.start('wayland', b'credential', ttl=.3, env=env)
        self.assertTrue(server.selected.wait(2))
        process.wait(timeout=2)
        self.assertNotEqual(process.returncode, 0)
        self.assertEqual(process.stdout.read(), b'')

    def test_unsupported_compositor_refuses(self):
        _, env = self.compositor(protocol=None)
        process = self.start('wayland', b'credential', env=env)
        process.wait(timeout=2)
        self.assertNotEqual(process.returncode, 0)
        self.assertEqual(process.stdout.read(), b'')

    def test_stopped_helper_rejects_late_read(self):
        server, env = self.compositor()
        process = self.start('wayland', b'credential', ttl=.3, env=env)
        self.ready(process)
        process.send_signal(signal.SIGSTOP)
        fd = server.request()
        time.sleep(.4)
        process.send_signal(signal.SIGCONT)
        process.wait(timeout=2)
        with os.fdopen(fd, 'rb') as pipe:
            self.assertEqual(pipe.read(), b'')

    def test_termination_removes_source(self):
        server, env = self.compositor()
        process = self.start('wayland', b'credential', ttl=10, env=env)
        self.ready(process)
        process.kill()
        process.wait(timeout=2)
        self.assertTrue(server.closed.wait(1))

    def test_replacement_preserved(self):
        server, env = self.compositor()
        process = self.start('wayland', b'credential', ttl=10, env=env)
        self.ready(process)
        server.event(server.source, 1)
        process.wait(timeout=2)
        self.assertEqual(server.selection_requests, [server.source])


class X11(Native):
    @classmethod
    def setUpClass(cls):
        xvfb = os.environ.get('ORG_GLANCE_TEST_XVFB') or shutil.which('Xvfb')
        if not xvfb or not shutil.which('xclip'):
            raise unittest.SkipTest('Xvfb and xclip are required for isolated X11 tests')
        read, write = os.pipe()
        cls.server = subprocess.Popen([xvfb, '-displayfd', str(write), '-screen', '0',
                                       '800x600x24', '-nolisten', 'tcp'], pass_fds=[write],
                                      stderr=subprocess.DEVNULL)
        os.close(write)
        if not select.select([read], [], [], 5)[0]:
            cls.server.terminate()
            raise RuntimeError('Xvfb did not start')
        display = os.read(read, 32).decode().strip()
        os.close(read)
        cls.environment = dict(os.environ, DISPLAY=':' + display)

    @classmethod
    def tearDownClass(cls):
        cls.server.terminate()
        cls.server.wait(timeout=3)

    def read_selection(self, selection='clipboard', target='UTF8_STRING'):
        return subprocess.run(['xclip', '-selection', selection, '-out', '-target', target],
                              env=self.environment, capture_output=True, timeout=2).stdout

    def owner(self, payload, selection='clipboard'):
        process = subprocess.Popen(['xclip', '-quiet', '-selection', selection, '-in'],
                                   stdin=subprocess.PIPE, stdout=subprocess.DEVNULL,
                                   stderr=subprocess.DEVNULL, env=self.environment)
        self.addCleanup(self.stop, process)
        process.stdin.write(payload)
        process.stdin.close()
        time.sleep(.05)
        return process

    def test_memory_transfer_and_primary_unchanged(self):
        self.owner(b'primary stays', 'primary')
        value = 'credential λ\nsecond line'.encode()
        process = self.start('x11', value, env=self.environment)
        self.ready(process)
        self.assertEqual(self.read_selection(), value)
        self.assertEqual(self.read_selection(target='x-kde-passwordManagerHint'), b'secret')
        self.assertEqual(self.read_selection('primary'), b'primary stays')
        process.wait(timeout=3)
        self.assertEqual(self.read_selection(), b'')

    def test_raw_payload_sizes_and_repeated_reads(self):
        for size in (0, 1, 3, 4, 16385, 262145):
            with self.subTest(size=size):
                value = (b'credential\n' * (size // 11 + 1))[:size]
                process = self.start('x11', value, ttl=3, env=self.environment)
                self.ready(process)
                for target in ('UTF8_STRING', 'text/plain', 'TEXT'):
                    self.assertEqual(self.read_selection(target=target), value)
                self.stop(process)

    def test_xlib_transport_never_copies_payload(self):
        compiler = shutil.which('cc')
        if not compiler:
            self.skipTest('A C compiler and Xlib development headers are required')
        with tempfile.TemporaryDirectory() as directory:
            library = Path(directory) / 'xlib-buffer.so'
            build = subprocess.run(
                [compiler, '-shared', '-fPIC', '-Wall', '-Wextra', '-Werror',
                 str(ROOT / 'test/clipboard-x11-buffer-probe.c'), '-o', str(library)],
                capture_output=True, text=True)
            if build.returncode and 'Xlibint.h: No such file' in build.stderr:
                self.skipTest('Xlib development headers are required')
            self.assertEqual(build.returncode, 0, build.stderr)
            probe = CDLL(str(library))
            probe.org_glance_test_xlib_buffer.argtypes = [c_void_p]
            probe.org_glance_test_xlib_buffer.restype = c_void_p
            probe.org_glance_test_xlib_buffer_size.argtypes = [c_void_p]
            probe.org_glance_test_xlib_buffer_size.restype = c_size_t
            with patch.dict(os.environ, self.environment):
                clipboard = Clipboard('fixture')
            self.addCleanup(clipboard.close)
            value = b'synthetic-xlib-transport-credential'
            secret = Secret(len(value))
            self.addCleanup(secret.close)
            secret.memory[:] = value
            target = clipboard.atoms['UTF8_STRING']
            writev = clipboard.xcb.xcb_writev

            def direct_write(connection, vectors, count, requests):
                self.assertEqual(vectors[1].base, secret.address)
                self.assertEqual(vectors[1].size, secret.size)
                return writev(connection, vectors, count, requests)

            with patch.object(clipboard.xcb, 'xcb_writev', side_effect=direct_write) as send:
                clipboard.respond(Request(30, 0, 0, clipboard.display, clipboard.window,
                                          clipboard.window, clipboard.atoms['CLIPBOARD'],
                                          target, target, 0), secret, Deadline(10))
                send.assert_called_once()
            clipboard.x.XSync(clipboard.display, 0)
            address = probe.org_glance_test_xlib_buffer(clipboard.display)
            size = probe.org_glance_test_xlib_buffer_size(clipboard.display)
            self.assertGreater(size, len(value))
            self.assertNotIn(value, string_at(address, size))

            def locked_kib(pointer):
                selected = False
                for line in Path('/proc/self/smaps').read_text().splitlines():
                    mapping = re.match(r'([0-9a-f]+)-([0-9a-f]+) ', line)
                    if mapping:
                        selected = int(mapping[1], 16) <= pointer < int(mapping[2], 16)
                    elif selected and line.startswith('Locked:'):
                        return int(line.split()[1])
                self.fail('No mapping found for the clipboard buffer')

            self.assertGreater(locked_kib(secret.address), 0)
            self.assertEqual(locked_kib(address), 0)

    def test_stopped_helper_rejects_late_read(self):
        process = self.start('x11', b'credential', ttl=.3, env=self.environment)
        self.ready(process)
        process.send_signal(signal.SIGSTOP)
        reader = subprocess.Popen(['xclip', '-selection', 'clipboard', '-out'],
                                  stdout=subprocess.PIPE, stderr=subprocess.DEVNULL,
                                  env=self.environment)
        self.addCleanup(self.stop, reader)
        time.sleep(.4)
        process.send_signal(signal.SIGCONT)
        process.wait(timeout=2)
        self.assertEqual(reader.communicate(timeout=2)[0], b'')

    def test_expired_reply_retires_owner_before_client_retry(self):
        with patch.dict(os.environ, self.environment):
            clipboard = Clipboard('fixture')
        self.addCleanup(clipboard.close)
        secret = Secret(10)
        self.addCleanup(secret.close)
        secret.memory[:] = b'credential'
        x, display = clipboard.x, clipboard.display
        requestor = x.XCreateSimpleWindow(
            display, x.XDefaultRootWindow(display), 0, 0, 1, 1, 0, 0, 0)
        selection = clipboard.atoms['CLIPBOARD']
        target = clipboard.atoms['UTF8_STRING']
        x.XSetSelectionOwner(display, selection, clipboard.window, 0)
        x.XSync(display, 0)
        clipboard.respond(Request(30, 0, 0, display, clipboard.window,
                                  requestor, selection, target, target, 0),
                          secret, Deadline(0))
        self.assertEqual(x.XGetSelectionOwner(display, selection), 0)
        self.assertGreater(x.XPending(display), 0)
        event = Event()
        x.XNextEvent(display, byref(event))
        self.assertEqual(event.type, 31)
        self.assertEqual(event.notification.property, 0)

    def test_termination_removes_source(self):
        process = self.start('x11', b'credential', ttl=10, env=self.environment)
        self.ready(process)
        process.kill()
        process.wait(timeout=2)
        self.assertEqual(self.read_selection(), b'')

    def test_replacement_preserved(self):
        process = self.start('x11', b'credential', ttl=1, env=self.environment)
        self.ready(process)
        self.owner(b'newer copy')
        process.wait(timeout=2)
        self.assertEqual(self.read_selection(), b'newer copy')

    def test_memory_lock_failure_preserves_previous_owner(self):
        self.owner(b'previous copy')
        process = self.start('x11', b'credential', env=self.environment,
                             preexec=lambda: resource.setrlimit(resource.RLIMIT_MEMLOCK, (0, 0)))
        process.wait(timeout=2)
        self.assertNotEqual(process.returncode, 0)
        self.assertEqual(process.stdout.read(), b'')
        self.assertEqual(self.read_selection(), b'previous copy')

    def test_stopped_emacs_does_not_extend_lease(self):
        if not shutil.which('emacs'):
            self.skipTest('Emacs is required')
        code = '''(progn (setq load-prefer-newer t)
          (require 'org-glance-clipboard)
          (let ((window-system 'x) (org-glance-clipboard-ttl 1))
            (org-glance-clipboard:copy "stopped-parent"))
          (write-region "parent-ready\\n" nil "/dev/stdout" nil 'silent)
          (sleep-for 30))'''
        process = subprocess.Popen(['emacs', '-Q', '--batch', '-L', str(HELPER.parent),
                                    '--eval', code], stdout=subprocess.PIPE,
                                   stderr=subprocess.DEVNULL, env=self.environment)
        self.addCleanup(self.stop, process)
        self.assertTrue(select.select([process.stdout], [], [], 5)[0])
        self.assertEqual(process.stdout.readline(), b'parent-ready\n')
        process.send_signal(signal.SIGSTOP)
        time.sleep(1.2)
        self.assertEqual(self.read_selection(), b'')


if __name__ == '__main__':
    unittest.main()
