/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package org.eolang.EO_sm.Posix; // NOPMD

import com.sun.jna.FunctionMapper;
import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;
import com.sun.jna.Pointer;
import com.sun.jna.Structure;
import com.sun.jna.ptr.IntByReference;
import java.util.Collections;
import org.eolang.EO_sm.SockaddrIn;

/**
 * C standard library with unix syscalls.
 * @since 0.40
 */
@SuppressWarnings("PMD.TooManyMethods")
public interface CStdLib extends Library {

    /**
     * C STDLIB instance.
     */
    CStdLib INSTANCE = CStdLib.load();

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
     * @param descriptor Old file descriptor
     * @return New file descriptor
     */
    int dup(int descriptor);

    /**
     * Duplicates a file descriptor to another.
     * @param descriptor Old file descriptor
     * @param other New file descriptor
     * @return Duplicated file descriptor
     * @checkstyle MethodNameCheck (5 lines)
     */
    int dup2(int descriptor, int other);

    /**
     * The "getpid" syscall.
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
     * @param descriptor File descriptor
     * @return Zero on success, -1 on error
     */
    int close(int descriptor);

    /**
     * Writes given bytes buffer to file descriptor.
     * @param descriptor File descriptor
     * @param buf Buffer
     * @param size Number of bytes to be written
     * @return Number of bytes was written
     */
    int write(int descriptor, byte[] buf, int size);

    /**
     * Read bytes from file descriptor.
     * @param descriptor File descriptor
     * @param buf Buffer
     * @param size Number of bytes to be read
     * @return Number of bytes was read
     */
    int read(int descriptor, byte[] buf, int size);

    /**
     * Check a file's accessibility.
     * @param path Path to the file
     * @param mode Accessibility check to perform (0 tests for existence)
     * @return Zero when the check succeeds, -1 on error
     */
    int access(String path, int mode);

    /**
     * Get file status by path.
     * @param path Path to the file
     * @param statbuf Structure to fill with the file's metadata
     * @return Zero on success, -1 on error
     */
    int stat(String path, Structure statbuf);

    /**
     * Delete a name from the filesystem.
     * @param path Path to the file
     * @return Zero on success, -1 on error
     */
    int unlink(String path);

    /**
     * Remove an empty directory.
     * @param path Path to the directory
     * @return Zero on success, -1 on error
     */
    int rmdir(String path);

    /**
     * Create a directory.
     * @param path Path to the directory
     * @param mode Permission bits for the new directory
     * @return Zero on success, -1 on error
     */
    int mkdir(String path, int mode);

    /**
     * Create a new file, or truncate an existing one, and open it.
     * @param path Path to the file
     * @param mode Permission bits for a newly created file
     * @return File descriptor on success, -1 on error
     */
    int creat(String path, int mode);

    /**
     * Rename a file, moving it between directories if required.
     * @param from Current path of the file
     * @param target New path of the file
     * @return Zero on success, -1 on error
     */
    int rename(String from, String target);

    /**
     * Get environment variable.
     * @param name Name of the variable
     * @return Name of the environment variable
     */
    String getenv(String name);

    /**
     * Get current time.
     * @param timeval Timevalue
     * @param timezone Timezone
     * @return Zero on success, -1 on error
     */
    int gettimeofday(GettimeofdaySyscall.Timeval timeval, Pointer timezone);

    /**
     * Create an endpoint for communication.
     * @param domain Socket domain
     * @param type Socket type
     * @param protocol Socket protocol
     * @return New socket descriptor on success, -1 on error
     */
    int socket(int domain, int type, int protocol);

    /**
     * Connects to the server at the specified IP address and port.
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return Zero on success, -1 on error
     */
    int connect(int sockfd, SockaddrIn addr, int addrlen);

    /**
     * Assigns the address specified by {@code addr} to the socket referred to
     * by the file descriptor {@code sockfd}.
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return Zero on success, -1 on error
     */
    int bind(int sockfd, SockaddrIn addr, int addrlen);

    /**
     * Listen for incoming connections on socket.
     * @param sockfd Socket descriptor
     * @param backlog Specifies the queue length for completely established
     *  sockets waiting to be accepted
     * @return Zero on success, -1 on error
     */
    int listen(int sockfd, int backlog);

    /**
     * Accept connection on socket.
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return On success, file descriptor for the accepted socket
     *  (a nonnegative integer) is returned. On error, -1 is returned
     */
    int accept(int sockfd, SockaddrIn addr, IntByReference addrlen);

    /**
     * Receive a message from a socket.
     * @param sockfd Socket descriptor
     * @param buf Byte buffer to store received bytes
     * @param len Size of received data
     * @param flags Flags
     * @return The number of received bytes on success, -1 on error
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    int recv(int sockfd, byte[] buf, int len, int flags);

    /**
     * Send a message to a socket.
     * @param sockfd Socket descriptor
     * @param buf Byte buffer to store sent bytes
     * @param len Size of sent data
     * @param flags Flags
     * @return The number of sent bytes on success, -1 on error
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    int send(int sockfd, byte[] buf, int len, int flags);

    /**
     * Convert IP string to binary form.
     * @param address IP address
     * @return IP address in binary form
     * @checkstyle MethodNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.MethodNamingConventions")
    int inet_addr(String address);

    /**
     * Converts {@code errno} to a human-readable string.
     * @param errno The error number
     * @return Error as string
     */
    String strerror(int errno);

    /**
     * Load the C standard library.
     *
     * <p>On Intel macOS, {@code dlsym("stat")} resolves to the legacy
     * 32-bit-inode version whose struct layout differs from the 64-bit-inode
     * one used by {@link StatSyscall.Mac}. We remap {@code stat} to
     * {@code stat$INODE64} to get the right layout. On arm64 macOS and
     * Linux the plain {@code stat} symbol already uses that layout.</p>
     *
     * @return Loaded CStdLib instance
     */
    private static CStdLib load() {
        final CStdLib result;
        if (Platform.isMac() && !Platform.isARM()) {
            result = Native.load(
                "c",
                CStdLib.class,
                Collections.singletonMap(
                    Library.OPTION_FUNCTION_MAPPER,
                    (FunctionMapper) (lib, method) -> {
                        final String mapped;
                        if ("stat".equals(method.getName())) {
                            mapped = "stat$INODE64";
                        } else {
                            mapped = method.getName();
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
