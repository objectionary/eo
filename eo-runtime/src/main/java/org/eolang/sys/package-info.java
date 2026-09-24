/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * EO runtime, what the syscall adapters share.
 *
 * <p>A {@code sockaddr_in} is laid out one way by Linux and Windows and
 * another way by macOS, and both families of adapters, posix and win32,
 * hand that layout to the kernel. It is theirs, not the runtime's, so it
 * lives here rather than in the root package next to {@code Phi} and
 * {@code Bytes}. The same goes for {@code Syscall}, the shape every
 * adapter takes, and for {@code Buffer}, {@code Handles}, {@code Cstring}
 * and {@code TupleToArray}, which size a read and carry a pointer, a text
 * and a tuple of arguments across to C on their behalf. Both families sit
 * under this package too, as {@code org.eolang.sys.posix} and
 * {@code org.eolang.sys.win32}, so a reader finds the adapters where what
 * they share already lives.</p>
 *
 * @since 0.77.0
 */
package org.eolang.sys;
