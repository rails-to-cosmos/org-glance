"""Own an X11 selection in a process whose reads enforce the lease deadline."""

import ctypes as c
import os
import select


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
        response = Event()
        response.notification = Notification(31, 0, 1, self.display, request.requestor,
                                             request.selection, request.target, 0, request.time)
        prop = request.property or request.target
        data, count, bits, kind = None, 0, 8, request.target
        if request.target == self.atoms['TARGETS']:
            offered = [value for key, value in self.atoms.items() if key not in ('ATOM', 'CLIPBOARD')]
            data = (c.c_ulong * len(offered))(*offered)
            count, bits, kind = len(offered), 32, self.atoms['ATOM']
        elif request.target == self.atoms['x-kde-passwordManagerHint']:
            data, count = c.c_char_p(b'secret'), 6
        elif request.target in [self.atoms[key] for key in ('UTF8_STRING', 'TEXT',
                                                           'text/plain;charset=utf-8', 'text/plain')]:
            data, count = c.c_void_p(secret.address), secret.size
            if request.target == self.atoms['TEXT']:
                kind = self.atoms['UTF8_STRING']
        remaining = deadline.remaining()
        if not remaining:
            self.retire()
        if data is not None and remaining:
            self.x.XChangeProperty(self.display, request.requestor, prop, kind,
                                   bits, 0, data, count)
            response.notification.property = prop
        self.x.XSendEvent(self.display, request.requestor, 0, 0, c.byref(response))
        self.x.XFlush(self.display)

    def retire(self):
        if self.window:
            self.x.XDestroyWindow(self.display, self.window)
            self.x.XSync(self.display, 0)
            self.window = 0

    def close(self):
        if self.display:
            self.x.XCloseDisplay(self.display)
            self.display = None
