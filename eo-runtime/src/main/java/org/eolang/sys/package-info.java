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
 * {@code Bytes}. The same goes for {@code Buffer}, {@code Handles},
 * {@code Cstring} and {@code TupleToArray}, which size a read and carry
 * a pointer, a text and a tuple of arguments across to C on their
 * behalf.</p>
 *
 * @since 0.77.0
 * @todo #8618:30min Move Syscall here too, together with SyscallTest,
 *  ReadSyscallTest and StatSyscallTest. It serves posix and win32 alone,
 *  yet standing in org.eolang it reads as part of the runtime's public
 *  surface, the way Bytes and PhDefault are. It waits on its own pull
 *  request because sixty-two adapters import it and the diff would not
 *  fit under the two-hundred-line limit alongside this one.
 * @todo #8533:30min Re-parent org.eolang.posix and org.eolang.win32 as
 *  org.eolang.sys.posix and org.eolang.sys.win32, and name the new directories
 *  in sonar.cpd.exclusions in pom.xml, which is the only place outside Java
 *  that spells the old ones out.
 */
package org.eolang.sys;
