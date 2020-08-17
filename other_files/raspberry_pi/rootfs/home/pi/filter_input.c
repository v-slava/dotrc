// apt-get install libevdev-dev
// gcc filter_input.c $(pkg-config --cflags --libs libevdev)

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <libevdev/libevdev.h>
#include <libevdev/libevdev-uinput.h>

#include <dirent.h>
#include <string.h>
#include <errno.h>
#include <stdio.h>
#include <assert.h>
#include <limits.h>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>

/* EVAL REGION BEGINS HERE: |* |
 * let g:My_eval_var = "MyRunShellCmd clang filter_input.c $(pkg-config
 * \ --cflags --libs libevdev) -o /tmp/filter_input && /tmp/filter_input"
 * EVAL REGION ENDS HERE. */

#define DEV_INPUT "/dev/input/"
#define UINPUT "/dev/uinput"

#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))
#define NO_RETURN __attribute__((noreturn))
#define STARTS_WITH(str, prefix) \
        (0 == strncmp((str), (prefix), sizeof(prefix) - 1))

static DIR *dev_input_dir = NULL;
static struct libevdev *devices[128] = {NULL};
static struct libevdev_uinput *uidev = NULL;

static void NO_RETURN cleanup(int exit_status)
{
    int ret;
    if (dev_input_dir != NULL) {
        errno = 0;
        ret = closedir(dev_input_dir);
        if (ret != 0)
            perror("closedir(" DEV_INPUT ")");
    }
    if (uidev != NULL) {
        int uifd = libevdev_uinput_get_fd(uidev);
        libevdev_uinput_destroy(uidev);
        ret = close(uifd);
        if (ret != 0)
            perror("close");
    }
    unsigned i;
    for (i = 0; i < ARRAY_SIZE(devices); i++) {
        if (devices[i] == NULL)
            break;
        int fd = libevdev_get_fd(devices[i]);
        libevdev_free(devices[i]);
        errno = 0;
        ret = close(fd);
        if (ret != 0)
            perror("close");
    }
    exit(exit_status);
}

/* Allow SIGTERM to cause graceful termination */
static void on_term(int s)
{
    cleanup(0);
}

int main(void)
{
    int ret;

    errno = 0;
    dev_input_dir = opendir(DEV_INPUT);
    if (dev_input_dir == NULL) {
        perror("opendir(" DEV_INPUT ")");
        return 1;
    }

    typedef void (*sighandler_t)(int);
    errno = 0;
    sighandler_t prev = signal(SIGTERM, on_term);
    if (prev == SIG_ERR) {
        const char *message = strerror(errno);
        fprintf(stderr, "Failed to set SIGTERM signal handler: %s\n", message);
        cleanup(1);
    }

    char evdev_file[PATH_MAX] = DEV_INPUT;
    unsigned dev_idx = 0;
    struct libevdev *event0_dev = NULL;
    while (1) {
        errno = 0;
        struct dirent *dir = readdir(dev_input_dir);
        if (dir == NULL) {
            if (errno == 0)
                break;
            perror("readdir(" DEV_INPUT ")");
            cleanup(1);
        }

        if (dir->d_type != DT_CHR)
            continue;
        if (STARTS_WITH(dir->d_name, "mice"))
            continue;
        if (STARTS_WITH(dir->d_name, "mouse"))
            continue;

        if (dev_idx >= ARRAY_SIZE(devices)) {
            fputs("Found too many input devices\n", stderr);
            cleanup(1);
        }

        static_assert(sizeof(evdev_file) >= sizeof(DEV_INPUT) - 1 + 256,
                "sizeof(evdev_file) is too small");
        strcpy(evdev_file + sizeof(DEV_INPUT) - 1, dir->d_name);

        errno = 0;
        int fd = open(evdev_file, O_RDONLY);
        if (fd < 0) {
            const char *message = strerror(errno);
            fprintf(stderr, "open(%s): %s\n", evdev_file, message);
            cleanup(1);
        }

        struct libevdev *dev = NULL;
        ret = libevdev_new_from_fd(fd, &dev);
        if (ret < 0) {
            fprintf(stderr, "Failed to init libevdev for %s: %s\n",
                    evdev_file, strerror(-ret));
            cleanup(1);
        }
        assert(dev != NULL);
        devices[dev_idx] = dev;
        if (0 == strcmp(dir->d_name, "event0"))
            event0_dev = dev;
        ++dev_idx;

        ret = libevdev_grab(dev, LIBEVDEV_GRAB);
        if (ret != 0) {
            fprintf(stderr, "Failed to grab %s: %s\n", evdev_file,
                    strerror(-ret));
            cleanup(1);
        }
    }

    errno = 0;
    ret = closedir(dev_input_dir);
    dev_input_dir = NULL;
    if (ret != 0) {
        perror("closedir(" DEV_INPUT ")");
        cleanup(1);
    }

    errno = 0;
    int uifd = open(UINPUT, O_RDWR);
    if (uifd < 0) {
        perror("open(" UINPUT ")");
        cleanup(1);
    }

    ret = libevdev_uinput_create_from_device(event0_dev, uifd, &uidev);
    if (ret != 0) {
        fprintf(stderr, "libevdev_uinput_create_from_device() failed: %s\n",
                strerror(-ret));
        cleanup(1);
    }

    puts("Listening for input events...");
    while (1) {
        struct input_event ev;
        memset(&ev, 0, sizeof(ev));
        ret = libevdev_next_event(event0_dev,
                LIBEVDEV_READ_FLAG_NORMAL | LIBEVDEV_READ_FLAG_BLOCKING, &ev);
        if (ret == -EAGAIN)
            continue;
        else if (ret == LIBEVDEV_READ_STATUS_SYNC)
            continue;
        else if (ret != LIBEVDEV_READ_STATUS_SUCCESS) {
            fprintf(stderr, "libevdev_next_event() failed: %s\n",
                    strerror(-ret));
            cleanup(1);
        }

        if (ret < 0) {
            fputs("error\n", stderr);
            cleanup(1);
        }

        // values: 0 = key up, 1 = key down, 2 = key hold.
        // type = EV_MSC = 4, code = MSC_SCAN = 4, value = 458834 (scan code)
        // type = EV_KEY = 1, code = KEY_UP = 103 | KEY_DOWN = 108, value = 0|1|2
        // type = EV_SYN = 0, code = SYN_REPORT = 0, value = 0
        if (ev.type != EV_KEY)
            continue;
        if ((ev.code != KEY_UP) && (ev.code != KEY_DOWN))
            continue;
        ret = libevdev_uinput_write_event(uidev, ev.type, ev.code, ev.value);
        if (ret != 0) {
            fprintf(stderr, "libevdev_uinput_write_event(ev) failed: %s\n",
                    strerror(-ret));
            cleanup(1);
        }
        ret = libevdev_uinput_write_event(uidev, EV_SYN, SYN_REPORT, 0);
        if (ret != 0) {
            fprintf(stderr, "libevdev_uinput_write_event(uidev, EV_SYN, "
                    "SYN_REPORT, 0) failed: %s\n", strerror(-ret));
            cleanup(1);
        }
    }
    cleanup(0);
    return 0;
}
