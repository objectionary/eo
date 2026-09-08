/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.win32;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.ptr.IntByReference;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;
import org.eolang.SockaddrIn;

/**
 * Interface definitions for <code>WS2_32.dll</code>.
 *
 * @since 0.40
 * @checkstyle AbbreviationAsWordInNameCheck (1000 lines)
 */
@SuppressWarnings("PMD.MethodNamingConventions")
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
    short VERSION_2_2 = (short) 0x0202;

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
    long INVALID_SOCKET = -1L;

    /**
     * Status returned on errors with socket.
     */
    int SOCKET_ERROR = -1;

    /**
     * Initializes winsock usage by DLL process.
     *
     * @param version Highest Windows socket specification version
     * @param data Data with info about socket structure
     * @return Zero on success, error code on error
     */
    int WSAStartup(short version, WSAData data);

    /**
     * Stops usage of Winsock 2 by DLL.
     *
     * @return Zero on success, SOCKET_ERROR on error
     */
    int WSACleanup();

    /**
     * Creates a socket.
     *
     * <p>The return type is {@link Pointer} rather than a fixed-width
     * primitive because a {@code SOCKET} is a Windows {@code UINT_PTR}:
     * 4 bytes on Win32, 8 on Win64 (#7577). JNA marshals {@link Pointer}
     * at the platform's actual pointer width, matching that either way.</p>
     *
     * @param domain Socket domain
     * @param type Socket type
     * @param protocol Socket protocol
     * @return Socket descriptor
     */
    Pointer socket(int domain, int type, int protocol);

    /**
     * Closes a socket.
     *
     * @param socket Socket descriptor
     * @return Zero on success, otherwise, a value of SOCKET_ERROR is returned
     */
    int closesocket(Pointer socket);

    /**
     * Connects to the server at the specified IP address and port.
     *
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return Zero on success, otherwise, a value of SOCKET_ERROR is returned
     */
    int connect(Pointer sockfd, SockaddrIn addr, int addrlen);

    /**
     * Assigns the address specified by {@code addr} to the socket referred to
     * by the file descriptor {@code sockfd}.
     *
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return Zero on success, -1 on error
     */
    int bind(Pointer sockfd, SockaddrIn addr, int addrlen);

    /**
     * Listen for incoming connections on socket.
     *
     * @param sockfd Socket descriptor
     * @param backlog Specifies the queue length for completely established sockets
     *  waiting to be accepted
     * @return Zero on success, -1 on error
     */
    int listen(Pointer sockfd, int backlog);

    /**
     * Accept connection on socket.
     *
     * @param sockfd Socket descriptor
     * @param addr Address structure
     * @param addrlen The size of the address structure
     * @return On success, file descriptor for the accepted socket (a nonnegative integer)
     *  is returned. On error, -1 is returned
     */
    Pointer accept(Pointer sockfd, SockaddrIn addr, IntByReference addrlen);

    /**
     * Send a message to a socket.
     *
     * @param sockfd Socket descriptor
     * @param buf Byte buffer to store sent bytes
     * @param len Size of sent data
     * @param flags Flags
     * @return The number of sent bytes on success, -1 on error
     */
    int send(Pointer sockfd, byte[] buf, int len, int flags);

    /**
     * Receive a message from a socket.
     *
     * @param sockfd Socket descriptor
     * @param buf Byte buffer to store received bytes
     * @param len Size of received data
     * @param flags Flags
     * @return The number of received bytes on success, -1 on error
     */
    int recv(Pointer sockfd, byte[] buf, int len, int flags);

    /**
     * Convert an IPv4 address from text into a 32-bit number in network byte
     * order.
     *
     * <p>Four forms are accepted — {@code a.b.c.d}, {@code a.b.c}, {@code a.b}
     * and {@code a} — and every part is read the way C reads a number: decimal,
     * octal behind a leading zero, or hexadecimal behind a leading {@code 0x}.
     * A part that is not the last one stands for one byte, and the last one
     * fills all the bytes still left, so {@code 127.1} is {@code 127.0.0.1}.</p>
     *
     * <p>The address arrives as NUL-terminated bytes and not as a
     * {@link String} because this library is loaded with
     * {@link W32APIOptions#DEFAULT_OPTIONS}, whose type mapper hands a
     * {@link String} over as {@code wchar_t*}. That is right for the wide
     * entry points of WS2_32 and wrong for this one, which takes a narrow
     * {@code const char*}: {@code 127.0.0.1} would reach it as UTF-16 and be
     * read as {@code 1}, the byte after the first one being NUL. An array
     * bypasses the mapper and is passed as the bytes it holds.</p>
     *
     * @param address The address to convert, NUL-terminated
     * @return The address in network byte order, or {@code INADDR_NONE} when
     *  the text is not one of those forms
     */
    int inet_addr(byte[] address);

    /**
     * Set the last winsock error. It is the winsock counterpart of Kernel32's
     * {@code SetLastError} and writes the same per-thread last-error slot.
     *
     * <p>There is no {@code WSAGetLastError} here on purpose. The slot it reads
     * is overwritten by the JNI and JNA machinery that stands between a mapped
     * call and the Java code around it, so a mapped {@code WSAGetLastError}
     * answers with whatever that machinery left behind, usually zero, instead
     * of the code the previous call set. The code is read back from
     * {@link com.sun.jna.Native#getLastError()}, which hands over the copy JNA
     * takes of the slot the instant every mapped call returns — this one
     * included. JNA short-circuits Kernel32's {@code GetLastError} to that same
     * copy for the same reason.</p>
     *
     * @param error The error code to set
     */
    void WSASetLastError(int error);
}
