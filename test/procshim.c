/* procshim.c — minimal POSIX process control for the two-process crash test
 * (test/proctest.f90). Kept OUT of the library (osshim.c / libsqr) on purpose:
 * sqr itself spawns no subprocess, and this machinery is needed only to run a
 * real second process that crashes with a hot journal for the recovery test.
 *
 * Not built on Windows (the proctest target is POSIX-only); the stubs below
 * just let it compile there. */

#include <stdint.h>

#ifndef _WIN32

#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

/* fork + execv `path` with argv = {path, arg1, NULL}. Returns the child pid,
 * or -1 on a fork failure. */
int64_t sqr_test_spawn(const char *path, const char *arg1) {
    pid_t pid = fork();
    if (pid < 0) return -1;
    if (pid == 0) {
        char *const argv[] = { (char *)path, (char *)arg1, (char *)0 };
        execv(path, argv);
        _exit(127);                 /* exec failed */
    }
    return (int64_t)pid;
}

/* Block until `pid` exits; return its exit status, or -1 if it did not exit
 * normally (killed by a signal) or on a waitpid error. */
int sqr_test_wait(int64_t pid) {
    int status;
    if (waitpid((pid_t)pid, &status, 0) < 0) return -1;
    if (WIFEXITED(status)) return WEXITSTATUS(status);
    return -1;
}

/* Terminate abruptly, bypassing Fortran finalizers and buffered-unit flush —
 * the stand-in for a crashed process holding a hot journal (no db_close). */
void sqr_test_hard_exit(int code) {
    _exit(code);
}

#else  /* _WIN32 — proctest is not built here; stubs keep the C compilable. */

int64_t sqr_test_spawn(const char *path, const char *arg1) {
    (void)path; (void)arg1; return -1;
}
int sqr_test_wait(int64_t pid) { (void)pid; return -1; }
void sqr_test_hard_exit(int code) { (void)code; }

#endif
