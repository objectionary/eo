/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * Lowering of EO formations into Java atoms.
 *
 * <p>A formation whose inputs are data and whose body is arithmetic,
 * comparisons and the like says the same thing twice: once in EO, where
 * every step of it is an object built and dataized while the program runs,
 * and once in the answer a compiler could have worked out beforehand. This
 * module works it out and keeps it as the body of a generated Java atom,
 * so that the graph behind such a formation is never built at all:</p>
 *
 * <pre> [a b] &gt; gap
 *   (a.minus b).times (a.minus b) &gt; @</pre>
 *
 * <p>Everything about the calculus lives in the external {@code phino}
 * binary, which this module drives through
 * {@link org.eolang.lowering.Phino} and trusts only at the pinned version.
 * The whole build goes through it as one document holding every object
 * of the world, because a formation of one file is copied by objects of
 * another and the two cannot be evaluated apart, and each entry of that
 * document is morphed by a run of its own.</p>
 *
 * <p>The work is a pipeline of stages, each a {@link org.cactoos.Proc}
 * over the directory of the build, composed by
 * {@link org.eolang.lowering.Lowering}: the tests are cut out, the entries
 * are written, the world is merged, every entry is morphed into a protocol
 * of its own, the sources are patched, and the Java is rendered. No stage
 * skips and no stage retries, so a build either folds what it says it
 * folded or fails.</p>
 *
 * @since 0.74.0
 * @see <a href="https://www.eolang.org">Project site www.eolang.org</a>
 * @see <a href="https://github.com/objectionary/eo">GitHub project</a>
 */
package org.eolang.lowering;
