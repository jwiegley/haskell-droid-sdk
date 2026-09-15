#define _GNU_SOURCE
#include <errno.h>
#include <stdint.h>
#include <unistd.h>

#if defined(__APPLE__)
#include <libproc.h>
#elif defined(__linux__)
#include <fcntl.h>
#include <inttypes.h>
#include <stdio.h>
#else
#error "File-position tests support macOS and Linux"
#endif

/* lseek(SEEK_CUR, 0) can overwrite a concurrent read's offset on macOS. */
int droid_test_fd_position(int fd, int64_t *position) {
#if defined(__APPLE__)
    struct vnode_fdinfo info;
    int count = proc_pidfdinfo(getpid(), fd, PROC_PIDFDVNODEINFO, &info, sizeof(info));
    if (count != (int)sizeof(info)) {
        if (count > 0) errno = EIO;
        return -1;
    }
    *position = info.pfi.fi_offset;
    return 0;
#else
    char path[64], buffer[128];
    snprintf(path, sizeof(path), "/proc/self/fdinfo/%d", fd);
    int source = open(path, O_RDONLY | O_CLOEXEC);
    if (source < 0) return -1;
    ssize_t count;
    do {
        count = read(source, buffer, sizeof(buffer) - 1);
    } while (count < 0 && errno == EINTR);
    int saved_errno = errno;
    close(source);
    if (count <= 0) {
        errno = count < 0 ? saved_errno : EIO;
        return -1;
    }
    buffer[count] = '\0';
    if (sscanf(buffer, "pos:%" SCNd64, position) != 1) {
        errno = EIO;
        return -1;
    }
    return 0;
#endif
}
