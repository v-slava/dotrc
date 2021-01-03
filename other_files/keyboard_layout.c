/*
The MIT License (MIT)
Copyright (c) 2016 Viacheslav Volkov

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// Compilation:
// gcc keyboard_layout.c -lX11 -O2 -flto -o /media/files/programs/keyboard_layout

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <X11/Xlib.h>
#include <X11/XKBlib.h>

static void PrintUsage(const char* program_name)
{
    assert(program_name != NULL);
    fprintf(stderr, "Usage: %s [--display DISPLAY] {--get-num-layouts | \
--set-layout {next | prev | LAYOUT_NUMBER}}\n", program_name);
}

static const char* GetProgramName(const char* argv0)
{
    assert(argv0 != NULL);
    char *ret = strrchr(argv0, '/');
    if(ret == NULL)
        return argv0;
    return ret + 1;
}

static int ParseArgs(int argc, char* argv[], const char** display_str,
    Bool* get_num_layouts, Bool* next, Bool* prev,
    Bool* layout_number_specified, unsigned* layout_number)
{
    assert(argc >= 1);
    assert(argv != NULL);
    assert(get_num_layouts != NULL);
    assert(next != NULL);
    assert(prev != NULL);
    assert(layout_number_specified != NULL);
    assert(layout_number != NULL);

    *get_num_layouts = False;
    *next = False;
    *prev = False;
    *layout_number_specified = False;

    const char* program_name = GetProgramName(argv[0]);
    int option_index = 1;
    int num_args_left = argc - 1;
    if (num_args_left == 0)
    {
        PrintUsage(program_name);
        return 1;
    }

    if (strcmp(argv[option_index], "--display") == 0)
    {
        ++option_index;
        --num_args_left;
        if (num_args_left == 0)
        {
            PrintUsage(program_name);
            return 1;
        }
        *display_str = argv[option_index];
        ++option_index;
        --num_args_left;
        if (num_args_left == 0)
        {
            PrintUsage(program_name);
            return 1;
        }
    }

    if (strcmp(argv[option_index], "--get-num-layouts") == 0)
    {
        *get_num_layouts = True;
        return 0;
    }

    if (strcmp(argv[option_index], "--set-layout") == 0)
    {
        ++option_index;
        --num_args_left;
        if (num_args_left == 0)
        {
            PrintUsage(program_name);
            return 1;
        }
        if (strcmp(argv[option_index], "next") == 0)
        {
            *next = True;
            return 0;
        }
        if (strcmp(argv[option_index], "prev") == 0)
        {
            *prev = True;
            return 0;
        }

        char* endptr;
        unsigned long layout = strtoul(argv[option_index], &endptr, 10);
        if ((*endptr) != 0)
        {
            PrintUsage(program_name);
            return 1;
        }
        if (layout > UINT_MAX)
        {
            fputs("Given LAYOUT_NUMBER is too big", stderr);
            return 2;
        }
        *layout_number = (unsigned)layout;
        *layout_number_specified = True;
        return 0;
    }

    PrintUsage(program_name);
    return 1;
}

#ifdef DEBUG_OUTPUT
#define DEBUG(format, ...) printf(format ".\n", ## __VA_ARGS__)
#else
#define DEBUG(format, ...)
#endif

#define ERROR(error_code, goto_label, format, ...) \
do {                                               \
    fprintf(stderr, format ".\n", ## __VA_ARGS__); \
    ret = error_code;                              \
    goto goto_label;                               \
} while (0)


int main(int argc, char *argv[])
{
    int ret;
    Status status;
    Bool succeeded;

    const char* display_str = NULL;
    Display *dpy;
    XkbDescPtr xkb;
    XkbStateRec xkb_state;
    unsigned num_groups;
    unsigned current_group;

    Bool get_num_layouts, next, prev, layout_number_specified;
    unsigned layout_number;

    ret = ParseArgs(argc, argv, &display_str, &get_num_layouts, &next, &prev,
        &layout_number_specified, &layout_number);
    if (ret != 0)
        goto no_cleanup;

    if (display_str == NULL)
        display_str = getenv("DISPLAY");
    if (display_str == NULL)
        display_str = ":0"; // ":0.0"
    dpy = XOpenDisplay(display_str);
    if (dpy == NULL)
    {
        ERROR(3, no_cleanup, "Can't open display: XOpenDisplay(\"%s\") failed",
                display_str);
    }

    xkb = XkbAllocKeyboard();
    if (xkb == NULL)
        ERROR(4, close_display, "XkbAllocKeyboard() failed");

    xkb->dpy = dpy;
    status = XkbGetControls(dpy, XkbAllControlsMask, xkb);
    if (status != Success)
        ERROR(5, free_keyboard, "XkbGetControls() failed");

    num_groups = xkb->ctrls->num_groups;
    DEBUG("num_groups = %u", num_groups);

    if (get_num_layouts)
    {
        printf("%u\n", num_groups);
        goto free_keyboard;
    }

    if (layout_number_specified)
    {
        if (layout_number >= num_groups)
            ERROR(2, free_keyboard, "Specified LAYOUT_NUMBER = %u is too big \
(it must be in range [0; %u])", layout_number, num_groups - 1);
        current_group = layout_number;
    }
    else
    {
        status = XkbGetState(dpy, XkbUseCoreKbd, &xkb_state);
        if (status != Success)
            ERROR(6, free_keyboard, "XkbGetState() failed");

        current_group = xkb_state.group;
        DEBUG("current_group (before) = %u", current_group);

        if (next)
            current_group = (current_group + 1) % num_groups;
        else if (prev)
        {
            current_group = (current_group > 0) ? (current_group - 1) :
                num_groups - 1;
        }

        DEBUG("current_group (after) = %u", current_group);
    }

    succeeded = XkbLockGroup(dpy, XkbUseCoreKbd, current_group);
    if (succeeded == False)
        ERROR(7, free_keyboard, "XkbLockGroup() failed");

free_keyboard:
    XkbFreeKeyboard(xkb, 0, True);
    // In XkbFreeKeyboard(xkb, which, free_all) above "which" is unused since
    // free_all == True.
close_display:
    XCloseDisplay(dpy);
no_cleanup:
    return ret;
}
