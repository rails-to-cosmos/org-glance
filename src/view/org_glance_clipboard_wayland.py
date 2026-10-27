"""Serve a Wayland data-control source directly from locked anonymous memory."""

import array
from collections import deque
import os
import select
import socket
import struct

from org_glance_clipboard import Deadline, send_fd


def words(*values):
    return struct.pack('=' + 'I' * len(values), *values)


def string(value):
    data = value.encode() + b'\0'
    return words(len(data)) + data + b'\0' * (-len(data) % 4)


def read_string(data, offset=0):
    size, = struct.unpack_from('=I', data, offset)
    end = offset + 4 + size
    if size < 1 or end > len(data) or data[end - 1] != 0:
        raise ValueError("Malformed Wayland string")
    return data[offset + 4:end - 1].decode(), end + (-size % 4)


class Clipboard:
    def __init__(self, token):
        self.marker = 'application/x-org-glance-' + token
        self.socket = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.socket.settimeout(5)
        path = os.environ.get('WAYLAND_DISPLAY', 'wayland-0')
        if not os.path.isabs(path):
            path = os.path.join(os.environ['XDG_RUNTIME_DIR'], path)
        self.socket.connect(path)
        self.buffer = bytearray()
        self.fds = deque()
        self.next_id = 2
        self.registry = self.new_id()
        self.manager = self.device = self.source = None
        self.globals = {}
        self.offers = {}
        self.callbacks = set()
        self.running = True
        self.published = False
        self.secret = self.deadline = None
        self.request(1, 1, words(self.registry))
        self.sync()
        for name in ('ext_data_control_manager_v1', 'zwlr_data_control_manager_v1'):
            if name in self.globals:
                self.manager = self.bind(name)
                break
        if self.manager is None or 'wl_seat' not in self.globals:
            raise RuntimeError("Wayland data-control is unavailable")
        seat = self.bind('wl_seat')
        self.device = self.new_id()
        self.request(self.manager, 1, words(self.device, seat))
        self.sync()

    def new_id(self):
        result = self.next_id
        self.next_id += 1
        return result

    def request(self, obj, opcode, payload=b''):
        self.socket.sendall(words(obj, ((len(payload) + 8) << 16) | opcode) + payload)

    def bind(self, name):
        global_id, _version = self.globals[name]
        obj = self.new_id()
        self.request(self.registry, 0, words(global_id) + string(name) + words(1, obj))
        return obj

    def sync(self):
        callback = self.new_id()
        self.callbacks.add(callback)
        self.request(1, 0, words(callback))
        startup = Deadline(5)
        while callback in self.callbacks:
            if not startup.remaining():
                raise TimeoutError("Wayland initialization timed out")
            self.receive(min(.05, startup.remaining()))

    def receive(self, timeout):
        if not select.select([self.socket], [], [], timeout)[0]:
            return
        data, ancillary, flags, _ = self.socket.recvmsg(65536, socket.CMSG_SPACE(256))
        for level, kind, raw in ancillary:
            if level == socket.SOL_SOCKET and kind == socket.SCM_RIGHTS:
                descriptors = array.array('i')
                descriptors.frombytes(raw[:len(raw) - len(raw) % descriptors.itemsize])
                self.fds.extend(descriptors)
        if not data or flags & socket.MSG_CTRUNC:
            raise EOFError("Wayland connection closed")
        self.buffer.extend(data)
        while len(self.buffer) >= 8:
            obj, header = struct.unpack_from('=II', self.buffer)
            size, opcode = header >> 16, header & 65535
            if size < 8 or size % 4:
                raise ValueError("Malformed Wayland message")
            if len(self.buffer) < size:
                break
            payload = bytes(self.buffer[8:size])
            del self.buffer[:size]
            self.event(obj, opcode, payload)

    def event(self, obj, opcode, data):
        if obj == 1:
            if opcode == 0:
                raise RuntimeError("Wayland protocol error")
        elif obj == self.registry and opcode == 0:
            name, = struct.unpack_from('=I', data)
            interface, offset = read_string(data, 4)
            version, = struct.unpack_from('=I', data, offset)
            self.globals.setdefault(interface, (name, version))
        elif obj in self.callbacks:
            self.callbacks.remove(obj)
        elif obj == self.device:
            if opcode == 0:
                offer, = struct.unpack('=I', data)
                self.offers[offer] = set()
            elif opcode in (1, 3):
                offer, = struct.unpack('=I', data)
                if opcode == 1:
                    ours = self.marker in self.offers.get(offer, ())
                    if self.published and not ours:
                        self.running = False
                    self.published = self.published or ours
                if offer:
                    self.request(offer, 1)
                    self.offers.pop(offer, None)
            elif opcode == 2:
                self.running = False
        elif obj == self.source:
            if opcode == 0:
                mime, _ = read_string(data)
                if not self.fds:
                    raise RuntimeError("Missing transfer descriptor")
                fd = self.fds.popleft()
                if mime in ('text/plain;charset=utf-8', 'text/plain'):
                    with memoryview(self.secret.memory)[:self.secret.size] as view:
                        send_fd(fd, view, self.deadline)
                else:
                    value = b'secret' if mime == 'x-kde-passwordManagerHint' else b''
                    send_fd(fd, value, self.deadline)
            elif opcode == 1:
                self.running = False
        elif obj in self.offers and opcode == 0:
            mime, _ = read_string(data)
            self.offers[obj].add(mime)

    def run(self, secret, deadline):
        self.secret, self.deadline = secret, deadline
        self.source = self.new_id()
        self.request(self.manager, 0, words(self.source))
        for mime in ('text/plain;charset=utf-8', 'text/plain',
                     'x-kde-passwordManagerHint', self.marker):
            self.request(self.source, 0, string(mime))
        self.request(self.device, 0, words(self.source))
        startup = Deadline(5)
        while self.running and not self.published and deadline.remaining() and startup.remaining():
            self.receive(min(.05, deadline.remaining(), startup.remaining()))
        if not self.published or not self.running or not deadline.remaining():
            raise RuntimeError("Wayland publication was not acknowledged")
        os.write(1, b'ready\n')
        while self.running and deadline.remaining():
            self.receive(min(.05, deadline.remaining()))

    def close(self):
        self.socket.close()
        while self.fds:
            os.close(self.fds.popleft())
