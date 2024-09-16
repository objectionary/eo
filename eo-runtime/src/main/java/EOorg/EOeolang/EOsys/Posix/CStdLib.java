/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
/*
 * @checkstyle PackageNameCheck (4 lines)
 * @checkstyle TrailingCommentCheck (3 lines)
 */
package EOorg.EOeolang.EOsys.Posix; // NOPMD

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Pointer;

/**
 * C standard library with unix syscalls.
 * @since 0.40
 */
public interface CStdLib extends Library {

    /**
     * C STDLIB instance.
     */
    CStdLib INSTANCE = Native.load("c", CStdLib.class);

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
     * @return Process ID.
     */
    int getpid();

    /**
     * The "open" syscall.
     * @param path Path to file to open
     * @param flags Open flags
     * @return File descriptor
     */
    int open(String path, int flags);

    /**
     * Close file descriptor.
     * @param descriptor File descriptor
     * @return Zero on success, -1 on error
     */
    int close(int descriptor);

    /**
     * Writes given bytes buffer to file descriptor.
     * @param descriptor File descriptor.
     * @param buf Buffer.
     * @param size Number of bytes to be written.
     * @return Number of bytes was written.
     */
    int write(int descriptor, String buf, int size);

    /**
     * Read bytes from file descriptor.
     * @param descriptor File descriptor.
     * @param buf Buffer.
     * @param size Number of bytes to be read.
     * @return Number of bytes was read.
     */
    int read(int descriptor, byte[] buf, int size);

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
     * Initiate a connection on a socket.
     * @param sockfd Socket descriptor.
     * @param addr Socket address structure
     * @param addrlen Size of socket address structure
     * @return Zero on success, -1 on error
     */
    int connect(int sockfd, ConnectSyscall.SockaddrIn addr, int addrlen);
}
