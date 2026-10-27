"""Serve X11 payloads directly from locked memory through XCB's raw socket API."""

import ctypes as c
import os
import select
import struct


class IOVec(c.Structure):
    _fields_ = [('base', c.c_void_p), ('size', c.c_size_t)]


class Request(c.Structure):
    _fields_ = [('type', c.c_int), ('serial', c.c_ulong), ('send_event', c.c_int),
                ('display', c.c_void_p), ('owner', c.c_ulong),
                ('requestor', c.c_ulong), ('selection', c.c_ulong),
                ('target', c.c_ulong), ('property', c.c_ulong), ('time', c.c_ulong)]


class Notification(c.Structure):
    _fields_ = Request._fields_[:4] + Request._fields_[5:]


class Event(c.Union):
    _fields_ = [('type', c.c_int), ('request', Request),
                ('notification', Notification), ('padding', c.c_long * 24)]


class Clipboard:
    def __init__(self, token):
        self.x = c.CDLL('libX11.so.6')
        self.bridge = c.CDLL('libX11-xcb.so.1')
        self.xcb = c.CDLL('libxcb.so.1')
        self.bridge.XGetXCBConnection.argtypes = [c.c_void_p]
        self.bridge.XGetXCBConnection.restype = c.c_void_p
        self.return_socket = c.CFUNCTYPE(None, c.c_void_p)(lambda _: None)
        self.xcb.xcb_take_socket.argtypes = [c.c_void_p, type(self.return_socket),
                                            c.c_void_p, c.c_int, c.POINTER(c.c_uint64)]
        self.xcb.xcb_take_socket.restype = c.c_int
        self.xcb.xcb_writev.argtypes = [c.c_void_p, c.POINTER(IOVec), c.c_int, c.c_uint64]
        self.xcb.xcb_writev.restype = c.c_int
        self.xcb.xcb_discard_reply64.argtypes = [c.c_void_p, c.c_uint64]
        self.xcb.xcb_discard_reply64.restype = None
        signatures = {
            'XOpenDisplay': (c.c_void_p, [c.c_char_p]),
            'XDefaultRootWindow': (c.c_ulong, [c.c_void_p]),
            'XCreateSimpleWindow': (c.c_ulong, [c.c_void_p, c.c_ulong, c.c_int, c.c_int,
                                               c.c_uint, c.c_uint, c.c_uint, c.c_ulong, c.c_ulong]),
            'XDestroyWindow': (c.c_int, [c.c_void_p, c.c_ulong]),
            'XInternAtom': (c.c_ulong, [c.c_void_p, c.c_char_p, c.c_int]),
            'XGetSelectionOwner': (c.c_ulong, [c.c_void_p, c.c_ulong]),
            'XSetSelectionOwner': (c.c_int, [c.c_void_p, c.c_ulong, c.c_ulong, c.c_ulong]),
            'XSync': (c.c_int, [c.c_void_p, c.c_int]),
            'XFlush': (c.c_int, [c.c_void_p]),
            'XPending': (c.c_int, [c.c_void_p]),
            'XNextEvent': (c.c_int, [c.c_void_p, c.POINTER(Event)]),
            'XConnectionNumber': (c.c_int, [c.c_void_p]),
            'XMaxRequestSize': (c.c_long, [c.c_void_p]),
            'XExtendedMaxRequestSize': (c.c_long, [c.c_void_p]),
            'XChangeProperty': (c.c_int, [c.c_void_p, c.c_ulong, c.c_ulong, c.c_ulong,
                                         c.c_int, c.c_int, c.c_void_p, c.c_int]),
            'XSendEvent': (c.c_int, [c.c_void_p, c.c_ulong, c.c_int, c.c_long, c.POINTER(Event)]),
            'XCloseDisplay': (c.c_int, [c.c_void_p]),
        }
        for name, (result, args) in signatures.items():
            function = getattr(self.x, name)
            function.restype, function.argtypes = result, args
        self.display = self.x.XOpenDisplay(None)
        if not self.display:
            raise RuntimeError('Cannot open X11 display')
        self.connection = self.bridge.XGetXCBConnection(self.display)
        if not self.connection:
            self.x.XCloseDisplay(self.display)
            raise RuntimeError('X11 raw transport is unavailable')
        self.error_handler = c.CFUNCTYPE(c.c_int, c.c_void_p, c.c_void_p)(lambda *_: 0)
        self.x.XSetErrorHandler(self.error_handler)
        self.window = self.x.XCreateSimpleWindow(
            self.display, self.x.XDefaultRootWindow(self.display), 0, 0, 1, 1, 0, 0, 0)
        self.atoms = {name: self.x.XInternAtom(self.display, name.encode(), 0)
                      for name in ('CLIPBOARD', 'TARGETS', 'ATOM', 'UTF8_STRING',
                                   'TEXT', 'text/plain;charset=utf-8', 'text/plain',
                                   'x-kde-passwordManagerHint')}

    def run(self, secret, deadline):
        maximum = self.x.XExtendedMaxRequestSize(self.display) or self.x.XMaxRequestSize(self.display)
        if secret.size > maximum * 4 - 128:
            raise ValueError('Property exceeds the X11 transfer limit')
        self.x.XSetSelectionOwner(self.display, self.atoms['CLIPBOARD'], self.window, 0)
        self.x.XSync(self.display, 0)
        if self.x.XGetSelectionOwner(self.display, self.atoms['CLIPBOARD']) != self.window:
            raise RuntimeError('X11 publication was not acknowledged')
        if not deadline.remaining():
            raise TimeoutError('Clipboard expired before publication')
        os.write(1, b'ready\n')
        while deadline.remaining():
            if not self.x.XPending(self.display):
                select.select([self.x.XConnectionNumber(self.display)], [], [],
                              min(.05, deadline.remaining()))
                continue
            event = Event()
            self.x.XNextEvent(self.display, c.byref(event))
            if event.type == 29:
                break
            if event.type == 30:
                self.respond(event.request, secret, deadline)
        if deadline.remaining():
            return
        self.retire()
        while self.x.XPending(self.display):
            event = Event()
            self.x.XNextEvent(self.display, c.byref(event))
            if event.type == 30:
                self.respond(event.request, secret, deadline)

    def respond(self, request, secret, deadline):
        prop = request.property or request.target
        data, count, bits, kind = None, 0, 8, request.target
        payload = False
        if request.target == self.atoms['TARGETS']:
            offered = [value for key, value in self.atoms.items() if key not in ('ATOM', 'CLIPBOARD')]
            data = (c.c_ulong * len(offered))(*offered)
            count, bits, kind = len(offered), 32, self.atoms['ATOM']
        elif request.target == self.atoms['x-kde-passwordManagerHint']:
            data, count = c.c_char_p(b'secret'), 6
        elif request.target in [self.atoms[key] for key in ('UTF8_STRING', 'TEXT',
                                                           'text/plain;charset=utf-8', 'text/plain')]:
            data, count = c.c_void_p(secret.address), secret.size
            payload = True
            if request.target == self.atoms['TEXT']:
                kind = self.atoms['UTF8_STRING']
        remaining = deadline.remaining()
        if not remaining:
            self.retire()
        written = False
        if data is not None and remaining:
            if payload:
                if self.write_payload(request, prop, kind, secret, deadline):
                    return
            else:
                self.x.XChangeProperty(self.display, request.requestor, prop, kind,
                                       bits, 0, data, count)
                written = True
        response = Event()
        response.notification = Notification(31, 0, 1, self.display, request.requestor,
                                             request.selection, request.target,
                                             prop if written else 0, request.time)
        self.x.XSendEvent(self.display, request.requestor, 0, 0, c.byref(response))
        self.x.XFlush(self.display)

    def write_payload(self, request, prop, kind, secret, deadline):
        """Send a property from SECRET's locked allocation without transport copies."""
        units = 6 + (secret.size + 3) // 4
        header = struct.pack('=BBHIIIB3xI', 18, 0, units if units <= 65535 else 0,
                             request.requestor, prop, kind, 8, secret.size)
        if units > 65535:
            header = header[:4] + struct.pack('=I', units + 1) + header[4:]
        # XCB requires a reply-producing first request to track sequence wraps.
        prefix = c.create_string_buffer(struct.pack('=BBH', 43, 0, 1) + header)
        padding = c.create_string_buffer(3)
        event = struct.pack('=BBHIIIII8x', 31, 0, 0, request.time, request.requestor,
                            request.selection, request.target, prop)
        suffix = c.create_string_buffer(
            struct.pack('=BBHII', 25, 0, 11, request.requestor, 0) + event)
        vectors = (IOVec * 4)(IOVec(c.addressof(prefix), len(prefix) - 1),
                             IOVec(secret.address, secret.size),
                             IOVec(c.addressof(padding), -secret.size % 4),
                             IOVec(c.addressof(suffix), len(suffix) - 1))
        sent = c.c_uint64()
        if not self.xcb.xcb_take_socket(self.connection, self.return_socket, None,
                                        0, c.byref(sent)):
            raise RuntimeError('Cannot acquire X11 transport')
        if not deadline.remaining():
            self.retire()
            return False
        if not self.xcb.xcb_writev(self.connection, vectors, 4, 3):
            raise RuntimeError('X11 payload transfer failed')
        self.xcb.xcb_discard_reply64(self.connection, sent.value + 1)
        return True

    def retire(self):
        if self.window:
            self.x.XDestroyWindow(self.display, self.window)
            self.x.XSync(self.display, 0)
            self.window = 0

    def close(self):
        if self.display:
            self.x.XCloseDisplay(self.display)
            self.display = None
