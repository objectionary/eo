/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * The engine that serves the atoms phino fires.
 *
 * <p>This is a program of its own and not a part of the build: the
 * {@code org.eolang.lowering} package writes a script that starts
 * {@link org.eolang.atoms.Engine} in a JVM of its own, hands the script
 * to phino as an atoms registry, and phino talks to it over stdin and
 * stdout, one JSON object per line. Nothing in the build ever calls a
 * class of this package, and nothing here ever touches a document.</p>
 *
 * <p>{@link org.eolang.atoms.Engine} reads the questions,
 * {@link org.eolang.atoms.Fires} picks the {@link org.eolang.atoms.Fire}
 * of the λ named, and the fire appends one row to the symbol table and
 * answers with a marker standing for that row, so phino goes on
 * rewriting over symbols. An operand that is not a value yet is asked
 * back of phino over the same wire, through
 * {@link org.eolang.atoms.Channel}, which suspends the fire until the
 * answer arrives.</p>
 *
 * @since 0.76.0
 * @see <a href="https://github.com/objectionary/phino">phino</a>
 * @see <a href="https://github.com/objectionary/eo">GitHub project</a>
 */
package org.eolang.atoms;
