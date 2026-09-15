#define _POSIX_C_SOURCE 200809L
#include "process-ipc.h"
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <unistd.h>

static int check_descriptors(void) {
  DIR *directory = opendir("/dev/fd");
  if (!directory) return 2;
  int result = 0;
  struct dirent *entry;
  while ((entry = readdir(directory))) {
    char *end;
    long fd = strtol(entry->d_name, &end, 10);
    if (*end || fd <= 2 || fd > INT_MAX || fd == dirfd(directory)) continue;
    if (fcntl((int)fd, F_GETFD) >= 0) {
      struct stat status;
      struct sockaddr_storage address;
      socklen_t size = sizeof(address);
      int family = getsockname((int)fd, (struct sockaddr *)&address, &size) == 0 ? address.ss_family : -1;
      if (fstat((int)fd, &status) == 0)
        fprintf(stderr, "Unexpected inherited descriptor: %ld mode=%o flags=%x family=%d device=%llu inode=%llu\n",
                fd, status.st_mode, fcntl((int)fd, F_GETFL), family,
                (unsigned long long)status.st_dev, (unsigned long long)status.st_ino);
      else fprintf(stderr, "Unexpected inherited descriptor: %ld\n", fd);
      result = 1;
    }
  }
  closedir(directory);
  return result;
}

int main(int argc, char **argv) {
  if (argc == 2 && strcmp(argv[1], "--check-fds") == 0) return check_descriptors();
  if (argc == 2 && strcmp(argv[1], "--stderr-state") == 0) {
    int flags = fcntl(STDERR_FILENO, F_GETFL);
    if (flags < 0) return 2;
    printf("{\"nonblocking\":%s}\n", flags & O_NONBLOCK ? "true" : "false");
    if (fflush(stdout)) return 2;
    char bytes[4096];
    memset(bytes, 'x', sizeof(bytes));
    for (unsigned i = 0; i < 64; i++) {
      size_t offset = 0;
      while (offset < sizeof(bytes)) {
        ssize_t written = write(STDERR_FILENO, bytes + offset, sizeof(bytes) - offset);
        if (written < 0 && errno == EINTR) continue;
        if (written <= 0) return 3;
        offset += (size_t)written;
      }
    }
    return 0;
  }
  if (argc == 2 && strcmp(argv[1], "--identity") == 0) {
    printf("{\"pid\":\"%ld\",\"group\":\"%ld\",\"session\":\"%ld\"}\n",
           (long)getpid(), (long)getpgrp(), (long)getsid(0));
    return 0;
  }
  if (argc < 6 || strcmp(argv[1], "--droid-ipc") != 0) return 2;
  unsigned int control;
  if (droid_ipc_bootstrap(&control)) return 3;
  if (signal(SIGTERM, SIG_IGN) == SIG_ERR) return 4;
  const char *path = getenv("DROID_IPC_PID_FILE");
  if (!path) return 5;
  FILE *file = fopen(path, "w");
  if (!file) return 6;
  if (fprintf(file, "%ld\n", (long)getpid()) < 0 || fclose(file)) return 7;
  /* Reach the real handoff, then require the parent's deadline/cleanup path. */
  sleep(15);
  return 0;
}
