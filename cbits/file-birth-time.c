#ifndef _GNU_SOURCE
#define _GNU_SOURCE 1
#endif
#ifndef _DARWIN_C_SOURCE
#define _DARWIN_C_SOURCE 1
#endif

#include <errno.h>
#include <stdint.h>

#if defined(__APPLE__)
#include <sys/stat.h>
#elif defined(__linux__)
#include <fcntl.h>
#include <linux/stat.h>
#include <sys/syscall.h>
#include <unistd.h>
#else
#error "File birth-time support requires macOS or Linux"
#endif

/* Return 1 for a reported birth time, 0 for unavailable, or -1 with errno. */
int hs_droid_file_birth_time(int fd, int64_t *seconds, int64_t *nanoseconds);

int hs_droid_file_birth_time(int fd, int64_t *seconds, int64_t *nanoseconds)
{
#if defined(__APPLE__)
    struct stat status;
    if (fstat(fd, &status) == -1) {
        return -1;
    }
    *seconds = (int64_t)status.st_birthtimespec.tv_sec;
    *nanoseconds = (int64_t)status.st_birthtimespec.tv_nsec;
    return 1;
#elif defined(SYS_statx) && defined(STATX_BTIME) && defined(AT_EMPTY_PATH)
    struct statx status;
    if (syscall(SYS_statx, fd, "", AT_EMPTY_PATH, STATX_BTIME, &status) == -1) {
        if (errno == ENOSYS || errno == EOPNOTSUPP) {
            return 0;
        }
        return -1;
    }
    if ((status.stx_mask & STATX_BTIME) == 0) {
        return 0;
    }
    *seconds = (int64_t)status.stx_btime.tv_sec;
    *nanoseconds = (int64_t)status.stx_btime.tv_nsec;
    return 1;
#else
    (void)fd;
    (void)seconds;
    (void)nanoseconds;
    return 0;
#endif
}
