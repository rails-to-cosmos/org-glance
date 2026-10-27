"""Run an acknowledged, memory-only clipboard lease independently of Emacs."""

import ctypes
import mmap
import os
import resource
import select
import stat
import sys
import time


class Deadline:
    def __init__(self, seconds, clock=None):
        self.clock = clock or (lambda: time.clock_gettime(time.CLOCK_BOOTTIME))
        self.end = self.clock() + seconds

    def remaining(self):
        return max(0.0, self.end - self.clock())


class Secret:
    def __init__(self, size):
        if not 0 <= size <= 16 * 1024 * 1024:
            raise ValueError("Unsupported clipboard size")
        self.size = size
        self.memory = mmap.mmap(-1, max(1, size),
                                flags=mmap.MAP_PRIVATE | mmap.MAP_ANONYMOUS)
        self.address = ctypes.addressof(ctypes.c_char.from_buffer(self.memory))
        self.libc = ctypes.CDLL(None, use_errno=True)
        self.libc.mlock.argtypes = [ctypes.c_void_p, ctypes.c_size_t]
        self.libc.munlock.argtypes = [ctypes.c_void_p, ctypes.c_size_t]
        if self.libc.mlock(self.address, len(self.memory)) != 0:
            self.memory.close()
            raise OSError("Cannot lock clipboard memory")

    def read(self, fd):
        deadline = Deadline(5)
        offset = 0
        os.set_blocking(fd, False)
        with memoryview(self.memory) as view:
            while offset < self.size:
                if not deadline.remaining():
                    raise TimeoutError("Clipboard input timed out")
                if not select.select([fd], [], [], min(.05, deadline.remaining()))[0]:
                    continue
                with view[offset:self.size] as part:
                    count = os.readv(fd, [part])
                if not count:
                    raise EOFError("Truncated clipboard input")
                offset += count

    def close(self):
        ctypes.memset(self.address, 0, len(self.memory))
        self.libc.munlock(self.address, len(self.memory))
        self.memory.close()


def send_fd(fd, data, deadline):
    """Serve only pipes and sockets, checking the lease before every write."""
    try:
        mode = os.fstat(fd).st_mode
        if not (stat.S_ISFIFO(mode) or stat.S_ISSOCK(mode)):
            return
        os.set_blocking(fd, False)
        offset = 0
        while offset < len(data) and deadline.remaining():
            if not select.select([], [fd], [], min(.05, deadline.remaining()))[1]:
                continue
            if not deadline.remaining():
                break
            try:
                offset += os.write(fd, data[offset:offset + 4096])
            except BlockingIOError:
                continue
    except (BrokenPipeError, OSError):
        pass
    finally:
        os.close(fd)


def main():
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    libc = ctypes.CDLL(None)
    if libc.prctl(4, 0, 0, 0, 0) != 0:
        raise OSError("Cannot disable process dumps")
    backend, size, seconds, token = sys.argv[1:]
    size, seconds = int(size), float(seconds)
    if seconds <= 0 or not seconds < float('inf'):
        raise ValueError("Invalid clipboard lifetime")
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    if backend == "wayland":
        from org_glance_clipboard_wayland import Clipboard
    elif backend == "x11":
        from org_glance_clipboard_x11 import Clipboard
    else:
        raise ValueError("Unknown clipboard backend")
    clipboard = Clipboard(token)
    secret = None
    try:
        secret = Secret(size)
        secret.read(0)
        clipboard.run(secret, Deadline(seconds))
    finally:
        clipboard.close()
        if secret is not None:
            secret.close()


if __name__ == '__main__':
    try:
        main()
    except Exception:
        sys.exit(1)
