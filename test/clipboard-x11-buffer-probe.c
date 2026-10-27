#include <X11/Xlibint.h>

const void *org_glance_test_xlib_buffer(Display *display)
{
    return display->buffer;
}

size_t org_glance_test_xlib_buffer_size(Display *display)
{
    return (size_t)(display->bufmax - display->buffer);
}
