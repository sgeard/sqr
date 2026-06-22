/*
 * osshim.c — platform filesystem / advisory-locking shim for sqr.
 *
 * The Fortran engine reaches every OS operation through a single set of
 * bind(c) interfaces (see clib_wrap). All platform divergence lives HERE,
 * behind the C preprocessor's _WIN32 — which a C compiler always predefines
 * correctly (unlike Fortran preprocessors, which predefine no platform macro
 * at all). Consequently the Fortran side needs no preprocessing, no -D flags
 * and no per-compiler build coaxing: it builds identically on every OS and
 * compiler, and `fpm test` works out of the box.
 *
 * On POSIX these are rename(2)/remove(3)/mkdir(2)/access(2)/nftw(3)/fsync(2)/
 * truncate(2)/flock(2)/isatty(3); on Windows the same operations map onto the
 * CRT and Win32 API (MoveFileExA gives rename's atomic replace, a
 * FindFirstFile recursion gives nftw's tree walk, LockFileEx gives flock's
 * advisory byte-range lock, _commit gives fsync, _chsize_s gives truncate).
 *
 * Every path arrives already NUL-terminated from the Fortran caller. Unless
 * noted, a function returns 0 on success and nonzero on failure. The lock
 * token is an opaque 64-bit value (a POSIX fd or a Win32 HANDLE); -1 means
 * "not held".
 */

/* Must precede every system header: on glibc it exposes nftw(3) and flock(2).
   Harmless on Windows/MinGW. */
#ifndef _GNU_SOURCE
#  define _GNU_SOURCE
#endif

#include <stdint.h>

#ifdef _WIN32
/* ===== Windows (CRT + Win32 API) ===== */
#include <windows.h>
#include <io.h>      /* _access, _open, _close, _commit, _chsize_s, _isatty */
#include <direct.h>  /* _mkdir */
#include <fcntl.h>   /* _O_RDWR */
#include <stdio.h>   /* remove, snprintf */
#include <string.h>  /* strcmp, memset */

int sqr_os_rename(const char *oldp, const char *newp) {
    /* CRT rename() refuses an existing target; MoveFileEx with
       REPLACE_EXISTING gives POSIX rename's atomic overwrite, which the
       compaction temp-file swap relies on. BOOL: nonzero = success. */
    return MoveFileExA(oldp, newp, MOVEFILE_REPLACE_EXISTING) ? 0 : 1;
}

int sqr_os_remove(const char *p) {
    return remove(p);
}

int sqr_os_mkdir(const char *p) {
    return _mkdir(p);           /* no mode argument on Windows */
}

int sqr_os_path_exists(const char *p) {
    return _access(p, 0) == 0 ? 1 : 0;   /* mode 0 == F_OK existence test */
}

/* Depth-first removal via the FindFirstFile family (no nftw on Windows):
   recurse into subdirectories, delete files, then remove the now-empty
   directory. Continue after a failure so a partial tree is cleared as far as
   possible, but report nonzero if any step failed. */
static int rmtree_win(const char *path) {
    char pat[MAX_PATH];
    WIN32_FIND_DATAA fd;
    HANDLE h;
    int ierr = 0;

    snprintf(pat, sizeof pat, "%s\\*", path);
    h = FindFirstFileA(pat, &fd);
    if (h == INVALID_HANDLE_VALUE) {
        /* No listing — remove the (presumably empty) directory itself. */
        return RemoveDirectoryA(path) ? 0 : 1;
    }
    do {
        const char *name = fd.cFileName;
        char child[MAX_PATH];
        if (strcmp(name, ".") == 0 || strcmp(name, "..") == 0) continue;
        snprintf(child, sizeof child, "%s\\%s", path, name);
        if (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
            if (rmtree_win(child) != 0) ierr = 1;
        } else {
            if (!DeleteFileA(child)) ierr = 1;
        }
    } while (FindNextFileA(h, &fd));
    FindClose(h);
    if (!RemoveDirectoryA(path)) ierr = 1;
    return ierr;
}

int sqr_os_rmtree(const char *p) {
    if (_access(p, 0) != 0) return 0;   /* nothing to remove */
    return rmtree_win(p);
}

int sqr_os_fsync_path(const char *p) {
    /* Read/write open so _commit (FlushFileBuffers) has a writable handle. */
    int fd = _open(p, _O_RDWR);
    int rc;
    if (fd < 0) return 1;
    rc = _commit(fd);
    if (_close(fd) != 0 && rc == 0) rc = 1;
    return rc;
}

int sqr_os_fsync_dir(const char *p) {
    (void)p;            /* NTFS journals directory metadata: nothing to do */
    return 0;
}

int sqr_os_truncate(const char *p, int64_t length) {
    int fd = _open(p, _O_RDWR);
    int rc;
    if (fd < 0) return 1;
    rc = _chsize_s(fd, length);
    if (_close(fd) != 0 && rc == 0) rc = 1;
    return rc;
}

int sqr_os_lock_try(const char *p, int exclusive, int64_t *tok) {
    OVERLAPPED ov;
    DWORD flags;
    /* The byte-range lock, not the share mode, provides exclusion, so the
       file is shared read+write; OPEN_ALWAYS creates it if absent. */
    HANDLE h = CreateFileA(p, GENERIC_READ | GENERIC_WRITE,
                           FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                           OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    *tok = -1;
    if (h == INVALID_HANDLE_VALUE) return 2;
    memset(&ov, 0, sizeof ov);          /* lock from offset 0 */
    flags = LOCKFILE_FAIL_IMMEDIATELY;  /* non-blocking */
    if (exclusive) flags |= LOCKFILE_EXCLUSIVE_LOCK;
    if (LockFileEx(h, flags, 0, 1, 0, &ov)) {
        *tok = (int64_t)(intptr_t)h;
        return 0;
    }
    CloseHandle(h);
    return 1;                           /* denied => contention */
}

void sqr_os_lock_release(int64_t *tok) {
    OVERLAPPED ov;
    HANDLE h;
    if (*tok < 0) return;
    h = (HANDLE)(intptr_t)*tok;
    memset(&ov, 0, sizeof ov);
    UnlockFileEx(h, 0, 1, 0, &ov);
    CloseHandle(h);                     /* closing also drops the lock */
    *tok = -1;
}

int sqr_os_lock_share(int64_t tok) {
    /* Windows has no atomic convert: drop the exclusive byte, then retake it
       shared. The brief gap is acceptable for a cooperative single-writer
       downgrade. */
    OVERLAPPED ov;
    HANDLE h;
    if (tok < 0) return 1;
    h = (HANDLE)(intptr_t)tok;
    memset(&ov, 0, sizeof ov);
    if (!UnlockFileEx(h, 0, 1, 0, &ov)) return 1;
    memset(&ov, 0, sizeof ov);
    return LockFileEx(h, LOCKFILE_FAIL_IMMEDIATELY, 0, 1, 0, &ov) ? 0 : 1;
}

int sqr_os_isatty_stdin(void) {
    return _isatty(0) ? 1 : 0;
}

#else
/* ===== POSIX (Linux, macOS, Android) ===== */
#include <stdio.h>          /* rename, remove */
#include <sys/stat.h>       /* mkdir */
#include <sys/types.h>
#include <unistd.h>         /* access, fsync, close, truncate, isatty */
#include <fcntl.h>          /* open, O_* */
#include <ftw.h>            /* nftw */
#include <sys/file.h>       /* flock */

int sqr_os_rename(const char *oldp, const char *newp) {
    return rename(oldp, newp);          /* already atomic-replace on POSIX */
}

int sqr_os_remove(const char *p) {
    return remove(p);
}

int sqr_os_mkdir(const char *p) {
    return mkdir(p, 0777);              /* 0777 & umask */
}

int sqr_os_path_exists(const char *p) {
    return access(p, F_OK) == 0 ? 1 : 0;
}

/* nftw callback: only the path is needed; remove() unlinks files and, because
   the walk is FTW_DEPTH (post-order), empty directories. Nonzero aborts. */
static int rm_entry(const char *fpath, const struct stat *sb,
                    int typeflag, struct FTW *ftwbuf) {
    (void)sb; (void)typeflag; (void)ftwbuf;
    return remove(fpath);
}

int sqr_os_rmtree(const char *p) {
    if (access(p, F_OK) != 0) return 0;   /* nothing to remove */
    /* FTW_DEPTH => contents before the directory, so remove() (which needs
       empty dirs) always succeeds bottom-up; FTW_PHYS => do not follow links. */
    return nftw(p, rm_entry, 16, FTW_PHYS | FTW_DEPTH);
}

/* Open `path`, fsync the fd, close it. fsync flushes the inode's dirty pages
   regardless of which descriptor opened it, so a fresh O_RDONLY fd suffices —
   for a directory fd this makes an entry creation/deletion durable too. */
static int fsync_via_open(const char *p) {
    int fd = open(p, O_RDONLY);
    int rc;
    if (fd < 0) return 1;
    rc = fsync(fd);
    if (close(fd) != 0 && rc == 0) rc = 1;
    return rc;
}

int sqr_os_fsync_path(const char *p) {
    return fsync_via_open(p);
}

int sqr_os_fsync_dir(const char *p) {
    return fsync_via_open(p);
}

int sqr_os_truncate(const char *p, int64_t length) {
    return truncate(p, (off_t)length);
}

int sqr_os_lock_try(const char *p, int exclusive, int64_t *tok) {
    int fd, op;
    *tok = -1;
    fd = open(p, O_RDWR | O_CREAT, 0644);
    if (fd < 0) return 2;               /* cannot open/create the lock file */
    op = (exclusive ? LOCK_EX : LOCK_SH) | LOCK_NB;
    if (flock(fd, op) == 0) {
        *tok = (int64_t)fd;
        return 0;
    }
    /* A non-blocking flock on a freshly-opened fd only fails because an
       incompatible lock is already held: treat it as contention. */
    close(fd);
    return 1;
}

void sqr_os_lock_release(int64_t *tok) {
    int fd;
    if (*tok < 0) return;
    fd = (int)*tok;
    flock(fd, LOCK_UN);                 /* explicit unlock; close also drops it */
    close(fd);
    *tok = -1;
}

int sqr_os_lock_share(int64_t tok) {
    if (tok < 0) return 1;
    /* flock converts an existing lock in place; non-blocking because a
       downgrade by the current holder never has to wait. */
    return flock((int)tok, LOCK_SH | LOCK_NB);
}

int sqr_os_isatty_stdin(void) {
    return isatty(0) ? 1 : 0;
}

#endif
