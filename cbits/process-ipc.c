#define _GNU_SOURCE
#include "process-ipc.h"
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <spawn.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <unistd.h>

#ifdef __APPLE__
#include <mach/mach.h>
#if __has_include(<sys/fileport.h>)
#include <sys/fileport.h>
#else
/* Older SDKs export this ABI but omit its header; declarations match SDK 26. */
typedef mach_port_t fileport_t;
extern int fileport_makeport(int, fileport_t *);
extern int fileport_makefd(fileport_t);
#endif /* fileport declarations */
#endif /* __APPLE__ */

#define CHANNELS 5
#define IPC_MAGIC 0x44524931U

struct droid_ipc_spawn {
  pid_t pid;
  int adopted;
  int phase;
  int fd[CHANNELS];
#ifdef __APPLE__
  mach_port_t control;
  mach_port_t bootstrap;
#else
  int control;
#endif
};

static void close_fd(int fd) {
  if (fd >= 0) close(fd);
}

#ifdef __APPLE__
struct port_packet {
  mach_msg_header_t header;
  mach_msg_body_t body;
  mach_msg_port_descriptor_t ports[CHANNELS];
};
struct port_receive {
  struct port_packet packet;
  mach_msg_max_trailer_t trailer;
};

static int send_ports(mach_port_t destination, const mach_port_t *ports,
                      unsigned count, mach_msg_type_name_t disposition, int tag) {
  struct port_packet packet = {0};
  packet.header.msgh_bits = MACH_MSGH_BITS(MACH_MSG_TYPE_COPY_SEND, 0) | MACH_MSGH_BITS_COMPLEX;
  packet.header.msgh_size = (mach_msg_size_t)(offsetof(struct port_packet, ports) + count * sizeof(packet.ports[0]));
  packet.header.msgh_remote_port = destination;
  packet.header.msgh_id = tag;
  packet.body.msgh_descriptor_count = count;
  for (unsigned i = 0; i < count; i++) {
    packet.ports[i].name = ports[i];
    packet.ports[i].disposition = disposition;
    packet.ports[i].type = MACH_MSG_PORT_DESCRIPTOR;
  }
  return mach_msg(&packet.header, MACH_SEND_MSG | MACH_SEND_TIMEOUT,
                  packet.header.msgh_size, 0, MACH_PORT_NULL, 0, MACH_PORT_NULL) == KERN_SUCCESS ? 0 : EIO;
}

static int receive_ports(mach_port_t source, mach_port_t *ports, unsigned count,
                         int tag, mach_msg_timeout_t timeout) {
  struct port_receive received = {0};
  mach_msg_return_t status = mach_msg(&received.packet.header, MACH_RCV_MSG | MACH_RCV_TIMEOUT,
      0, sizeof(received), source, timeout, MACH_PORT_NULL);
  if (status == MACH_RCV_TIMED_OUT) return EAGAIN;
  if (status != KERN_SUCCESS) return EIO;
  struct port_packet *p = &received.packet;
  if (p->header.msgh_id != tag || !(p->header.msgh_bits & MACH_MSGH_BITS_COMPLEX) ||
      p->body.msgh_descriptor_count != count ||
      p->header.msgh_size != offsetof(struct port_packet, ports) + count * sizeof(p->ports[0])) {
    mach_msg_destroy(&p->header);
    return EIO;
  }
  for (unsigned i = 0; i < count; i++) {
    if (p->ports[i].type != MACH_MSG_PORT_DESCRIPTOR) {
      mach_msg_destroy(&p->header);
      return EIO;
    }
  }
  for (unsigned i = 0; i < count; i++) ports[i] = p->ports[i].name;
  return 0;
}
#else
struct fd_packet {
  uint32_t magic;
  uint32_t mask;
};

static int send_fds(int control, const int *fds) {
  struct fd_packet packet = {IPC_MAGIC, 0};
  int supplied[CHANNELS];
  unsigned count = 0;
  for (unsigned i = 0; i < CHANNELS; i++) {
    if (fds[i] >= 0) {
      packet.mask |= 1U << i;
      supplied[count++] = fds[i];
    }
  }
  union { struct cmsghdr alignment; unsigned char bytes[CMSG_SPACE(sizeof(supplied))]; } ancillary = {0};
  struct iovec iov = {&packet, sizeof(packet)};
  struct msghdr message = {0};
  message.msg_iov = &iov;
  message.msg_iovlen = 1;
  message.msg_control = ancillary.bytes;
  message.msg_controllen = CMSG_SPACE(count * sizeof(int));
  struct cmsghdr *header = CMSG_FIRSTHDR(&message);
  header->cmsg_level = SOL_SOCKET;
  header->cmsg_type = SCM_RIGHTS;
  header->cmsg_len = CMSG_LEN(count * sizeof(int));
  memcpy(CMSG_DATA(header), supplied, count * sizeof(int));
  ssize_t sent;
  do { sent = sendmsg(control, &message, MSG_NOSIGNAL); } while (sent < 0 && errno == EINTR);
  return sent == sizeof(packet) ? 0 : EIO;
}

static int receive_fds(int control, int *fds) {
  struct fd_packet packet = {0};
  int supplied[CHANNELS];
  unsigned count = 0;
  union { struct cmsghdr alignment; unsigned char bytes[CMSG_SPACE(sizeof(supplied))]; } ancillary = {0};
  struct iovec iov = {&packet, sizeof(packet)};
  struct msghdr message = {0};
  message.msg_iov = &iov;
  message.msg_iovlen = 1;
  message.msg_control = ancillary.bytes;
  message.msg_controllen = sizeof(ancillary.bytes);
  ssize_t length = recvmsg(control, &message, MSG_DONTWAIT | MSG_CMSG_CLOEXEC);
  if (length < 0) return errno == EINTR ? EAGAIN : errno;
  for (struct cmsghdr *h = CMSG_FIRSTHDR(&message); h; h = CMSG_NXTHDR(&message, h)) {
    if (h->cmsg_level == SOL_SOCKET && h->cmsg_type == SCM_RIGHTS) {
      unsigned n = (unsigned)((h->cmsg_len - CMSG_LEN(0)) / sizeof(int));
      if (n > CHANNELS - count) n = CHANNELS - count;
      memcpy(supplied + count, CMSG_DATA(h), n * sizeof(int));
      count += n;
    }
  }
  if (length != sizeof(packet) || packet.magic != IPC_MAGIC ||
      (packet.mask != 27 && packet.mask != 31) ||
      count != (packet.mask == 31 ? 5U : 4U) || (message.msg_flags & (MSG_TRUNC | MSG_CTRUNC))) {
    for (unsigned i = 0; i < count; i++) close_fd(supplied[i]);
    return EIO;
  }
  unsigned index = 0;
  for (unsigned i = 0; i < CHANNELS; i++) fds[i] = packet.mask & (1U << i) ? supplied[index++] : -1;
  return 0;
}
#endif

void droid_ipc_free(struct droid_ipc_spawn *state) {
  if (!state) return;
  if (!state->adopted && state->pid > 0) {
    kill(state->pid, SIGKILL);
    while (waitpid(state->pid, NULL, 0) < 0 && errno == EINTR) {}
  }
  for (unsigned i = 0; i < CHANNELS; i++) close_fd(state->fd[i]);
#ifdef __APPLE__
  if (state->bootstrap != MACH_PORT_NULL) mach_port_deallocate(mach_task_self(), state->bootstrap);
  if (state->control != MACH_PORT_NULL) {
    mach_port_deallocate(mach_task_self(), state->control);
    mach_port_mod_refs(mach_task_self(), state->control, MACH_PORT_RIGHT_RECEIVE, -1);
  }
#else
  close_fd(state->control);
#endif
  free(state);
}

int droid_ipc_begin(const char *launcher, char *const argv[], char *const envp[],
                    int stderr_fd, int flags, struct droid_ipc_spawn **result) {
  struct droid_ipc_spawn *state = calloc(1, sizeof(*state));
  if (!state) return ENOMEM;
  state->pid = -1;
  for (unsigned i = 0; i < CHANNELS; i++) state->fd[i] = -1;
#ifndef __APPLE__
  state->control = -1;
  int pair[2] = {-1, -1};
#endif
  posix_spawnattr_t attributes;
  posix_spawn_file_actions_t actions;
  int has_attributes = 0, has_actions = 0, error = 0;
#define TRY(call) do { error = (call); if (error) goto failed; } while (0)
  TRY(posix_spawnattr_init(&attributes));
  has_attributes = 1;
  TRY(posix_spawn_file_actions_init(&actions));
  has_actions = 1;
  short spawn_flags = POSIX_SPAWN_SETSIGMASK | POSIX_SPAWN_SETSIGDEF;
  sigset_t empty, defaults;
  sigemptyset(&empty);
  sigemptyset(&defaults);
  sigaddset(&defaults, SIGINT);
  sigaddset(&defaults, SIGQUIT);
  sigaddset(&defaults, SIGPIPE);
  TRY(posix_spawnattr_setsigmask(&attributes, &empty));
  TRY(posix_spawnattr_setsigdefault(&attributes, &defaults));
  if (flags & 1) {
    spawn_flags |= POSIX_SPAWN_SETPGROUP;
    TRY(posix_spawnattr_setpgroup(&attributes, 0));
  }
  if (flags & 2) spawn_flags |= POSIX_SPAWN_SETSID;
  /* stderr may borrow fd 0 or 1; map it before replacing either. */
  if (stderr_fd == -2) {
#ifdef __APPLE__
    TRY(posix_spawn_file_actions_addinherit_np(&actions, 2));
#endif
  } else if (stderr_fd >= 0) {
    TRY(posix_spawn_file_actions_adddup2(&actions, stderr_fd, 2));
  } else {
    TRY(posix_spawn_file_actions_addopen(&actions, 2, "/dev/null", O_WRONLY, 0));
  }
  TRY(posix_spawn_file_actions_addopen(&actions, 0, "/dev/null", O_RDONLY, 0));
  TRY(posix_spawn_file_actions_addopen(&actions, 1, "/dev/null", O_WRONLY, 0));
#ifdef __APPLE__
  if (mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &state->control) != KERN_SUCCESS) { error = EIO; goto failed; }
  if (mach_port_insert_right(mach_task_self(), state->control, state->control, MACH_MSG_TYPE_MAKE_SEND) != KERN_SUCCESS) {
    mach_port_mod_refs(mach_task_self(), state->control, MACH_PORT_RIGHT_RECEIVE, -1);
    state->control = MACH_PORT_NULL;
    error = EIO;
    goto failed;
  }
  if (task_get_bootstrap_port(mach_task_self(), &state->bootstrap) != KERN_SUCCESS) { error = EIO; goto failed; }
  TRY(posix_spawnattr_setspecialport_np(&attributes, state->control, TASK_BOOTSTRAP_PORT));
  spawn_flags |= POSIX_SPAWN_CLOEXEC_DEFAULT;
#else
  if (socketpair(AF_UNIX, SOCK_SEQPACKET | SOCK_CLOEXEC, 0, pair) < 0) { error = errno; goto failed; }
  state->control = pair[0];
  TRY(posix_spawn_file_actions_adddup2(&actions, pair[1], 4));
  TRY(posix_spawn_file_actions_addclose(&actions, 3));
  TRY(posix_spawn_file_actions_addclosefrom_np(&actions, 5));
#endif
  TRY(posix_spawnattr_setflags(&attributes, spawn_flags));
  TRY(posix_spawn(&state->pid, launcher, &actions, &attributes, argv, envp));
  *result = state;
failed:
#ifndef __APPLE__
  close_fd(pair[1]);
#endif
  if (has_actions) posix_spawn_file_actions_destroy(&actions);
  if (has_attributes) posix_spawnattr_destroy(&attributes);
  if (error) droid_ipc_free(state);
  return error;
#undef TRY
}

int droid_ipc_pid(const struct droid_ipc_spawn *state) { return state->pid; }
void droid_ipc_adopt(struct droid_ipc_spawn *state) { state->adopted = 1; }
int droid_ipc_take_fd(struct droid_ipc_spawn *state, int index) {
  int fd = state->fd[index];
  state->fd[index] = -1;
  return fd;
}

int droid_ipc_poll(struct droid_ipc_spawn *state) {
  int error;
#ifdef __APPLE__
  if (state->phase == 0) {
    mach_port_t reply;
    error = receive_ports(state->control, &reply, 1, 1, 0);
    if (error) return error == EAGAIN ? 0 : -error;
    error = send_ports(reply, &state->bootstrap, 1, MACH_MSG_TYPE_COPY_SEND, 2);
    mach_port_deallocate(mach_task_self(), reply);
    if (error) return -error;
    state->phase = 1;
  }
  if (state->phase == 1) {
    mach_port_t ports[CHANNELS];
    error = receive_ports(state->control, ports, CHANNELS, 3, 0);
    if (error) return error == EAGAIN ? 0 : -error;
    error = 0;
    for (unsigned i = 0; i < CHANNELS; i++) {
      if (ports[i] != MACH_PORT_NULL) {
        state->fd[i] = fileport_makefd(ports[i]);
        if (state->fd[i] < 0) error = errno;
        mach_port_deallocate(mach_task_self(), ports[i]);
      } else if (i != 2) error = EIO;
    }
    if (error) return -error;
    state->phase = 2;
#else
  if (state->phase == 0) {
    error = receive_fds(state->control, state->fd);
    if (error) return error == EAGAIN || error == EWOULDBLOCK ? 0 : -error;
    state->phase = 2;
#endif
    int flags = fcntl(state->fd[4], F_GETFL);
    if (flags < 0 || fcntl(state->fd[4], F_SETFL, flags | O_NONBLOCK) < 0) return -errno;
  }
  int exec_error;
  ssize_t count = read(state->fd[4], &exec_error, sizeof(exec_error));
  if (count < 0) return errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR ? 0 : -errno;
  if (count == 0) return 1;
  return count == sizeof(exec_error) && exec_error > 0 ? -exec_error : -EIO;
}

int droid_ipc_bootstrap(unsigned int *parent) {
#ifdef __APPLE__
  mach_port_t control, reply, bootstrap;
  if (task_get_bootstrap_port(mach_task_self(), &control) != KERN_SUCCESS) return EIO;
  if (mach_port_allocate(mach_task_self(), MACH_PORT_RIGHT_RECEIVE, &reply) != KERN_SUCCESS) return EIO;
  if (send_ports(control, &reply, 1, MACH_MSG_TYPE_MAKE_SEND, 1)) return EIO;
  if (receive_ports(reply, &bootstrap, 1, 2, 30000)) return EIO;
  if (task_set_bootstrap_port(mach_task_self(), bootstrap) != KERN_SUCCESS) return EIO;
  mach_port_deallocate(mach_task_self(), bootstrap);
  mach_port_mod_refs(mach_task_self(), reply, MACH_PORT_RIGHT_RECEIVE, -1);
  *parent = control;
#else
  *parent = 4;
#endif
  return 0;
}

int droid_ipc_main(int argc, char **argv) {
  if (argc < 6 || strcmp(argv[1], "--droid-ipc") != 0) return 127;
  int pipe_stderr = strcmp(argv[2], "pipe") == 0;
  if (!pipe_stderr && strcmp(argv[2], "inherit") != 0 && strcmp(argv[2], "closed") != 0) return 127;
  if (strcmp(argv[3], "cwd") != 0 && strcmp(argv[3], "inherit-cwd") != 0) return 127;
  unsigned int control;
  if (droid_ipc_bootstrap(&control)) return 127;
  int pipes[CHANNELS][2];
  for (unsigned i = 0; i < CHANNELS; i++) pipes[i][0] = pipes[i][1] = -1;
  for (unsigned i = 0; i < CHANNELS; i++) {
    if (i == 2 && !pipe_stderr) continue;
    if ((i == 3 ? socketpair(AF_UNIX, SOCK_STREAM, 0, pipes[i]) : pipe(pipes[i])) < 0) return 127;
  }
  int parents[CHANNELS] = {pipes[0][1], pipes[1][0], pipes[2][0], pipes[3][0], pipes[4][0]};
  int children[4] = {pipes[0][0], pipes[1][1], pipes[2][1], pipes[3][1]};
  int status = pipes[4][1];
  if (fcntl(status, F_SETFD, FD_CLOEXEC) < 0) return 127;
#ifdef __APPLE__
  mach_port_t ports[CHANNELS] = {0};
  for (unsigned i = 0; i < CHANNELS; i++) {
    if (parents[i] >= 0 && fileport_makeport(parents[i], &ports[i])) return 127;
  }
  if (send_ports(control, ports, CHANNELS, MACH_MSG_TYPE_MOVE_SEND, 3)) return 127;
  mach_port_deallocate(mach_task_self(), control);
#else
  if (send_fds(4, parents)) return 127;
  close(4);
#endif
  for (unsigned i = 0; i < CHANNELS; i++) close_fd(parents[i]);
  for (unsigned i = 0; i < 4; i++) {
    if (children[i] >= 0 && children[i] != (int)i && dup2(children[i], (int)i) < 0) goto exec_failed;
  }
  for (unsigned i = 0; i < 4; i++) if (children[i] > 3) close_fd(children[i]);
  if (strcmp(argv[2], "closed") == 0) close(2);
  if (strcmp(argv[3], "cwd") == 0 && chdir(argv[4]) < 0) goto exec_failed;
  if (setenv("NODE_CHANNEL_FD", "3", 1) < 0 || setenv("NODE_CHANNEL_SERIALIZATION_MODE", "json", 1) < 0) goto exec_failed;
  execvp(argv[5], argv + 5);
exec_failed: {
    int error = errno ? errno : EIO;
    ssize_t written;
    do { written = write(status, &error, sizeof(error)); } while (written < 0 && errno == EINTR);
    return 127;
  }
}
