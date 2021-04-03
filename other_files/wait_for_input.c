#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <errno.h>
#include <dirent.h>
#include <fcntl.h>
#include <unistd.h>
#include <poll.h>

#define DEV_INPUT "/dev/input/"
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))

int main(void)
{
    errno = 0;
    DIR *dev_input_dir = opendir(DEV_INPUT);
    if (dev_input_dir == NULL) {
        perror("opendir(" DEV_INPUT ")");
        return 1;
    }

    int ret, main_ret = 1;
    struct pollfd poll_fds[256];
    char evdev_file[PATH_MAX] = DEV_INPUT;
    unsigned num_fds = 0;
    while (1) {
        errno = 0;
        struct dirent *dir = readdir(dev_input_dir);
        if (dir == NULL) {
            if (errno == 0)
                break; // end of directory stream
            perror("readdir(" DEV_INPUT ")");
            goto cleanup;
        }
        if (dir->d_type != DT_CHR)
            continue;
        if (num_fds == ARRAY_SIZE(poll_fds)) {
            fputs("Error: too many device files in " DEV_INPUT, stderr);
            goto cleanup;
        }
        strcpy(evdev_file + sizeof(DEV_INPUT) - 1, dir->d_name);
        errno = 0;
        int fd = open(evdev_file, O_RDONLY);
        if (fd < 0) {
            fprintf(stderr, "open(%s): %s\n", evdev_file, strerror(errno));
            goto cleanup;
        }
        poll_fds[num_fds].fd = fd;
        poll_fds[num_fds].events = POLLIN;
        ++num_fds;
    }

    do {
        int infinite_timeout = -1;
        errno = 0;
        ret = poll(poll_fds, num_fds, infinite_timeout);
        if (ret < 0) {
            perror("poll");
            goto cleanup;
        }
        // (ret == 0) means that poll() system call timed out before any file
        // descriptors became read.
    } while (ret == 0);
    // If we reach this line, either mouse has been moved or some keyboard
    // button has been pressed.
    main_ret = 0;

cleanup:
    errno = 0;
    ret = closedir(dev_input_dir);
    if (ret != 0) {
        perror("closedir(" DEV_INPUT ")");
        main_ret = 1;
    }
    for (unsigned i = 0; i < num_fds; i++) {
        int fd = poll_fds[i].fd;
        if (fd == 0)
            break;
        errno = 0;
        ret = close(fd);
        if (ret != 0) {
            perror("close");
            main_ret = 1;
        }
    }
    return main_ret;
}
