/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * Lowering of EO fragments into Java atoms through phino.
 *
 * <p>This module computes the fragments of an EO program at build time,
 * so that the object graph they describe is never built at runtime. The
 * work of φ-calculus — normalization and dataization — is delegated to
 * the external {@code phino} binary, pinned to one exact version; nothing
 * here decides what may be lowered, and nothing here rewrites: the module
 * prepares what phino reads, serves the λ functions phino fires, and
 * renders what comes back.</p>
 *
 * <p>A fragment is a named formation that declares arguments.
 * {@link org.eolang.lowering.Planted} finds them all and mints one λ name
 * per fragment into {@link org.eolang.lowering.Boxes}, and
 * {@link org.eolang.lowering.Boxed} writes a copy of every document with
 * that λ planted, so that entering a fragment fires the engine.
 * {@link org.eolang.lowering.Lowered} takes one document and runs phino
 * once per fragment of it: the boxed copies are merged into one universe,
 * {@link org.eolang.lowering.Registry} writes the {@code atoms.json} that
 * hands our λ functions to phino, and
 * {@link org.eolang.lowering.Applied} spells the way from {@code Φ} into
 * the fragment, with every void on the way applied to a marker of a fresh
 * symbol. {@link org.eolang.lowering.Phino} is the only class that runs
 * the binary, under a budget of steps and of seconds.</p>
 *
 * <p>phino serves the λ it fires through a process of its own, and
 * that process is not this package: {@link org.eolang.lowering.Registry}
 * writes a script that starts {@link org.eolang.atoms.Engine} in another
 * JVM, and everything the engine does lives in
 * {@code org.eolang.atoms}. What the two sides share are the files they
 * both read: {@link org.eolang.lowering.Symbols} holds the rows a fire
 * appends, {@link org.eolang.lowering.Boxes} the λ name of every
 * fragment, and {@link org.eolang.lowering.Marker} is the shape of the
 * answer a fire gives back.</p>
 *
 * <p>What comes back is a residual φ-expression full of markers, and the
 * table of symbols behind them is the straight-line program the fragment
 * really is. {@link org.eolang.lowering.Splice} puts the residual back
 * into the document it came from, keeping the names and places the
 * printer lives on, and {@link org.eolang.lowering.Marked} turns every
 * marker into a call of an atom whose body
 * {@link org.eolang.lowering.Table} reads out of the symbols and
 * {@link org.eolang.lowering.JavaAtom} renders into Java, saved as a
 * {@link org.eolang.lowering.Sidecar}. A fragment phino refuses — an
 * unknown λ, an error path, an exhausted budget — is simply left as
 * written, and the next one is tried.</p>
 *
 * @since 0.76.0
 * @see <a href="https://github.com/objectionary/phino">phino</a>
 * @see <a href="https://github.com/objectionary/eo">GitHub project</a>
 */
package org.eolang.lowering;
