/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.posix;

import com.sun.jna.FunctionMapper;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.ptr.IntByReference;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

/**
 * C standard library with unix syscalls.
 *
 * @since 0.40
 */
public interface CStdLib extends Library {

    /**
     * C STDLIB instance.
     */
    CStdLib INSTANCE = CStdLib.load();

    /**
     * The names macOS keeps for the calls that see a 64-bit inode.
     *
     * <p>On the Intel build of macOS the plain symbols are the ones from
     * before 64-bit inodes, and the current calls carry the {@code $INODE64}
     * suffix. The Apple silicon build has never had the old ones, so the
     * suffix is only ever added there where it exists.</p>
     */
    List<String> INODE64 = Arrays.asList("stat", "lstat", "opendir", "readdir");

    /**
     * Standard input file descriptor.
     */
    int STDIN_FILENO = 0;

    /**
     * Standard output file descriptor.
     */
    int STDOUT_FILENO = 1;

    /**
     * Open flag for reading only.
     */
    int O_RDONLY = 0;

    /**
     * Open flag for reading and writing.
     */
    int O_RDWR = 2;

    /**
     * TCP connection family.
     */
    int AF_INET = 2;

    /**
     * The "Socket as stream" type.
     */
    int SOCK_STREAM = 1;

    /**
     * Protocol for TCP connection.
     */
    int IPPROTO_TCP = 6;

    /**
     * Duplicates file descriptor.
     *
     * @param descriptor Old file descriptor
     * @return New file descriptor
     */
    int dup(int descriptor);

    /**
     * Duplicates a file descriptor to another.
     *
     * @param descriptor Old file descriptor
     * @param other New file descriptor
     * @return Duplicated file descriptor
     */
    int dup2(int descriptor, int other);

    /**
     * The "getpid" syscall.
     *
     * @return Process ID
     */
    int getpid();

    /**
     * The "open" syscall.
     *
     * <p>The native {@code open} is variadic, so {@code mode} is declared as a
     * trailing varargs: that makes JNA use the variadic calling convention,
     * without which the creation mode is read from the wrong place on some ABIs
     * (notably arm64, where variadic arguments are passed on the stack).</p>
     *
     * @param path Path to file to open
     * @param flags Open flags
     * @param mode Permission bits used when the flags request file creation
     * @return File descriptor
     */
    int open(String path, int flags, Object... mode);

    /**
     * Close file descriptor.
     *
     * @param descriptor File descriptor
     * @return Zero on success, -1 on error
     */
    int close(int descriptor);

    /**
     * Writes given bytes buffer to file descriptor.
     *
     * @param descriptor File descriptor
     * @param buf Buffer
     * @param size Number of bytes to be written
     * @return Number of bytes was written
     */
    int write(int descriptor, byte[] buf, int size);

    /**
     * Read bytes from file descriptor.
     *
     * @param descriptor File descriptor
     * @param buf Buffer
     * @param size Number of bytes to be read
     * @return Number of bytes was read
     */
    int read(int descriptor, byte[] buf, int size);

    /**
     * Check a file's accessibility.
     *
     * @param path Path to the file
     * @param mode Accessibility check to perform (0 tests for existence)
     * @return Zero when the check succeeds, -1 on error
     */
    int access(String path, int mode);

    /**
     * Get file status by path.
     *
     * @param path Path to the file
     * @param statbuf Structure to fill with the file's metadata
     * @return Zero on success, -1 on error
     */
    int stat(String path, Structure statbuf);

    /**
     * Get file status by path, without following a symbolic link.
     *
     * @param path Path to the file
     * @param statbuf Structure to fill with the file's metadata
     * @return Zero on success, -1 on error
     */
    int lstat(String path, Structure statbuf);

    /**
     * Delete a name from the filesystem.
     *
     * @param path Path to the file
     * @return Zero on success, -1 on error
     */
    int unlink(String path);

    /**
     * Remove an empty directory.
     *
     * @param path Path to the directory
     * @return Zero on success, -1 on error
     */
    int rmdir(String path);

    /**
     * Create a directory.
     *
     * @param path Path to the directory
     * @param mode Permission bits for the new directory
     * @return Zero on success, -1 on error
     */
    int mkdir(String path, int mode);

    /**
     * Create a new file, or truncate an existing one, and open it.
     *
     * @param path Path to the file
     * @param mode Permission bits for a newly created file
     * @return File descriptor on success, -1 on error
     */
    int creat(String path, int mode);

    /**
     * Rename a file, moving it between directories if required.
     *
     * @param from Current path of the file
     * @param target New path of the file
     * @return Zero on success, -1 on error
     */
    int rename(String from, String target);

    /**
     * Create a symbolic link pointing at a file or a directory.
     *
     * @param target Path the link leads to
     * @param path Path of the link itself
     * @return Zero on success, -1 on error
     */
    int symlink(String target, String path);

    /**
     * Get environment variable.
     *
     * @param name Name of the variable
     * @return Name of the environment variable
     */
    String getenv(String name);

    /**
     * Get current time.
     *
     * @param timeval Timevalue
     * @param timezone Timezone
     * @return Zero on success, -1 on error
     */
    int gettimeofday(Timeval timeval, Pointer timezone);

    /**
     * Create an endpoint for communication.
     *
     * @param domain Socket domain
     * @param type Socket type
     * @param protocol Socket protocol
     * @return New socket descriptor on success, -1 on error
     */
    int socket(int domain, int type, int protocol);

    /**
     * Connects to the server at the specified IP address and port.
     *
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return Zero on success, -1 on error
     */
    int connect(int sockfd, Structure addr, int addrlen);

    /**
     * Assigns the address specified by {@code addr} to the socket referred to
     * by the file descriptor {@code sockfd}.
     *
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return Zero on success, -1 on error
     */
    int bind(int sockfd, Structure addr, int addrlen);

    /**
     * Listen for incoming connections on socket.
     *
     * @param sockfd Socket descriptor
     * @param backlog Specifies the queue length for completely established
     *  sockets waiting to be accepted
     * @return Zero on success, -1 on error
     */
    int listen(int sockfd, int backlog);

    /**
     * Accept connection on socket.
     *
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return On success, file descriptor for the accepted socket
     *  (a nonnegative integer) is returned. On error, -1 is returned
     */
    int accept(int sockfd, Structure addr, IntByReference addrlen);

    /**
     * Receive a message from a socket.
     *
     * @param sockfd Socket descriptor
     * @param buf Byte buffer to store received bytes
     * @param len Size of received data
     * @param flags Flags
     * @return The number of received bytes on success, -1 on error
     */
    int recv(int sockfd, byte[] buf, int len, int flags);

    /**
     * Send a message to a socket.
     *
     * @param sockfd Socket descriptor
     * @param buf Byte buffer to store sent bytes
     * @param len Size of sent data
     * @param flags Flags
     * @return The number of sent bytes on success, -1 on error
     */
    int send(int sockfd, byte[] buf, int len, int flags);

    /**
     * Convert IP string to binary form.
     *
     * @param address IP address
     * @return IP address in binary form
     */
    @SuppressWarnings("PMD.MethodNamingConventions")
    int inet_addr(String address);

    /**
     * Open a directory for reading.
     *
     * @param path Path to the directory
     * @return Pointer to the directory stream, or NULL on error
     */
    Pointer opendir(String path);

    /**
     * Read the next entry of a directory stream.
     *
     * <p>The pointer leads to a {@code struct dirent} owned by libc, valid
     * until the next call on the same stream, so whatever is read out of it
     * has to be read at once.</p>
     *
     * @param dirp The directory stream
     * @return Pointer to the entry, or NULL when the stream is over
     */
    Pointer readdir(Pointer dirp);

    /**
     * Close a directory stream.
     *
     * @param dirp The directory stream
     * @return Zero on success, -1 on error
     */
    int closedir(Pointer dirp);

    /**
     * Converts {@code errno} to a human-readable string.
     *
     * @param errno The error number
     * @return Error as string
     */
    String strerror(int errno);

    private static CStdLib load() {
        final CStdLib result;
        if (Platform.isMac() && !Platform.isARM()) {
            result = Native.load(
                "c",
                CStdLib.class,
                Collections.singletonMap(
                    Library.OPTION_FUNCTION_MAPPER,
                    (FunctionMapper) (lib, method) -> {
                        final String name = method.getName();
                        final String mapped;
                        if (CStdLib.INODE64.contains(name)) {
                            mapped = String.format("%s$INODE64", name);
                        } else {
                            mapped = name;
                        }
                        return mapped;
                    }
                )
            );
        } else {
            result = Native.load("c", CStdLib.class);
        }
        return result;
    }
}
