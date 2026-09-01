/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * Lowering of pure EO fragments through phino.
 *
 * <p>This module computes pure fragments of an EO program at build time,
 * so that the object graph they describe is never built at runtime. The
 * work of φ-calculus — normalization and dataization — is delegated to
 * the external {@code phino} binary, pinned to one exact version; this
 * module only prepares what phino reads and interprets what it prints.</p>
 *
 * <p>A fragment arrives as XMIR: an application whose every leaf is a
 * literal, such as {@code 1.plus 1}. {@link org.eolang.lowering.MiniDoc}
 * renders it as the {@code φ} of a small self-contained φ-calculus
 * document, whose other bindings are the method tables of the primitive
 * λ-atoms — {@code number.plus}, {@code bytes.slice} and the rest — under
 * the short names phino registers for them.
 * {@link org.eolang.lowering.Phino} feeds that document to
 * {@code phino dataize} under a step budget and validates that what came
 * back is data. {@link org.eolang.lowering.Constant} puts the two
 * together and names the forma of the result, so that the caller can
 * splice a literal of the right shape where the fragment stood. A
 * fragment phino refuses — an unknown method, an error path, an
 * exhausted budget — is simply not folded, and the caller keeps the
 * original.</p>
 *
 * @since 0.76.0
 * @see <a href="https://github.com/objectionary/phino">phino</a>
 * @see <a href="https://github.com/objectionary/eo">GitHub project</a>
 */
package org.eolang.lowering;
