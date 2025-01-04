/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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
 * @checkstyle PackageNameCheck (20 lines)
 * @checkstyle TrailingCommentCheck (20 lines)
 */
package EOorg.EOeolang.EOsys.Win32; // NOPMD

import EOorg.EOeolang.EOsys.SockaddrIn;
import com.sun.jna.Native;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

/**
 * Interface definitions for <code>WS2_32.dll</code>.
 * @since 0.40
 * @checkstyle MethodNameCheck (1000 lines)
 * @checkstyle ParameterNumberCheck (1000 lines)
 * @checkstyle AbbreviationAsWordInNameCheck (1000 lines)
 */
@SuppressWarnings({"PMD.MethodNamingConventions", "PMD.TooManyMethods"})
public interface Winsock extends StdCallLibrary {
    /**
     * Instance.
     */
    Winsock INSTANCE = Native.load("ws2_32", Winsock.class, W32APIOptions.DEFAULT_OPTIONS);

    /**
     * Error code that indicates that an invalid argument was passed to the function.
     */
    int WSAEINVAL = 10_022;

    /**
     * Winsock version.
     */
    @SuppressWarnings("PMD.LongVariable")
    short WINSOCK_VERSION_2_2 = (short) 0x0202;

    /**
     * The Internet Protocol version 4 (IPv4) address family.
     */
    int AF_INET = 2;

    /**
     * A socket type that provides sequenced, reliable, two-way, connection-based byte streams
     * with an OOB data transmission mechanism.
     * This socket type uses the TCP for the Internet address family (AF_INET or AF_INET6).
     */
    int SOCK_STREAM = 1;

    /**
     * The Transmission Control Protocol (TCP). This is a possible value when
     * the af parameter is AF_INET or AF_INET6 and the type parameter is SOCK_STREAM.
     */
    int IPPROTO_TCP = 6;

    /**
     * Invalid socket descriptor.
     */
    int INVALID_SOCKET = -1;

    /**
     * Status returned on errors with socket.
     */
    int SOCKET_ERROR = -1;

    /**
     * Initializes winsock usage by DLL process.
     * @param version Highest Windows socket specification version.
     * @param data Data with info about socket structure
     * @return Zero on success, error code on error.
     */
    int WSAStartup(short version, WSAStartupFuncCall.WSAData data);

    /**
     * Stops usage of Winsock 2 by DLL.
     * @return Zero on success, SOCKET_ERROR on error.
     */
    int WSACleanup();

    /**
     * Creates a socket.
     * @param domain Socket domain
     * @param type Socket type
     * @param protocol Socket protocol
     * @return Socket descriptor
     */
    int socket(int domain, int type, int protocol);

    /**
     * Closes a socket.
     * @param socket Socket descriptor
     * @return Zero on success, otherwise, a value of SOCKET_ERROR is returned.
     */
    int closesocket(int socket);

    /**
     * Connects to the server at the specified IP address and port.
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return Zero on success, otherwise, a value of SOCKET_ERROR is returned.
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
     * @param backlog Specifies the queue length for completely established sockets
     *  waiting to be accepted
     * @return Zero on success, -1 on error
     */
    int listen(int sockfd, int backlog);

    /**
     * Accept connection on socket.
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return On success, file descriptor for the accepted socket (a nonnegative integer)
     *  is returned. On error, -1 is returned.
     */
    int accept(int sockfd, SockaddrIn addr, IntByReference addrlen);

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
     * Retrieve the last error from winsock.
     * @return The code of the last winsock error.
     */
    int WSAGetLastError();
}
