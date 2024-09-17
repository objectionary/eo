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
 * @checkstyle PackageNameCheck (20 lines)
 * @checkstyle TrailingCommentCheck (20 lines)
 */
package EOorg.EOeolang.EOsys.Win32; // NOPMD

import com.sun.jna.Native;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;

/**
 * Interface definitions for <code>WS2_32.dll</code>.
 * @since 0.40
 * @checkstyle MethodNameCheck (1000 lines)
 * @checkstyle ParameterNumberCheck (1000 lines)
 * @checkstyle AbbreviationAsWordInNameCheck (1000 lines)
 */
@SuppressWarnings("PMD.MethodNamingConventions")
public interface Winsock extends StdCallLibrary {
    /**
     * Instance.
     */
    Winsock INSTANCE = Native.load("Ws2_32", Winsock.class, W32APIOptions.DEFAULT_OPTIONS);

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
     * The inet_addr function converts a string containing an IPv4 dotted-decimal
     * address into a proper address for the {@link ConnectFuncCall.SockaddrIn} structure.
     * @param address IPv4 address
     * @return
     */
    int inet_addr(String address);

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
     * Establishes a connection to a specified socket.
     * @param socket Socket descriptor
     * @param addr Sockaddr structure
     * @param addrlen Sockaddr structure size
     * @return Zero on success, SOCKET_ERROR on error.
     */
    int connect(int socket, ConnectFuncCall.SockaddrIn addr, int addrlen);
}
