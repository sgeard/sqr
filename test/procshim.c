/* procshim.c — minimal process control for the two-process crash test
 * (test/proctest.f90). Kept OUT of the library (osshim.c / libsqr) on purpose:
 * sqr itself spawns no subprocess, and this machinery is needed only to run a
 * real second process that crashes with a hot journal for the recovery test.
 *
 * The Windows half does the same with _spawnl / _cwait / TerminateProcess,
 * for the cross-built proctest.exe (`make windows`). */

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

#else  /* _WIN32 — the same three calls on the C runtime and Win32. */

#include <process.h>
#include <windows.h>

/* Start `path` with argv = {path, arg1} and do not wait. Returns the process
 * handle (which is what sqr_test_wait takes here), or -1 on failure. Neither
 * string is quoted: the test passes its own path and a one-word mode. */
int64_t sqr_test_spawn(const char *path, const char *arg1) {
    return (int64_t)_spawnl(_P_NOWAIT, path, path, arg1, (char *)0);
}

/* Block until the process exits; return its exit status, or -1 on error. */
int sqr_test_wait(int64_t pid) {
    int status;
    if (_cwait(&status, (intptr_t)pid, 0) == -1) return -1;
    return status;
}

/* Terminate abruptly: no atexit handlers, no Fortran unit flush. */
void sqr_test_hard_exit(int code) {
    TerminateProcess(GetCurrentProcess(), (UINT)code);
}

#endif
