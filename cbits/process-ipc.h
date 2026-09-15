#ifndef DROID_PROCESS_IPC_H
#define DROID_PROCESS_IPC_H

struct droid_ipc_spawn;

int droid_ipc_begin(const char *, char *const[], char *const[], int, int,
                    struct droid_ipc_spawn **);
int droid_ipc_pid(const struct droid_ipc_spawn *);
void droid_ipc_adopt(struct droid_ipc_spawn *);
int droid_ipc_poll(struct droid_ipc_spawn *);
int droid_ipc_take_fd(struct droid_ipc_spawn *, int);
void droid_ipc_free(struct droid_ipc_spawn *);
int droid_ipc_bootstrap(unsigned int *);
int droid_ipc_main(int, char **);

#endif
