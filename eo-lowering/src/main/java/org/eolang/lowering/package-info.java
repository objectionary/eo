/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * Turning some EO objects into Java code before the program runs.
 *
 * <p>Some EO objects only do arithmetic, comparisons and similar work on
 * the data they are given. Here is an example:</p>
 *
 * <pre> [a b] &gt; gap
 *   (a.minus b).times (a.minus b) &gt; @</pre>
 *
 * <p>When the program runs, EO builds many small objects to compute the
 * result of such an object. That is slow. This module finds such objects
 * while the program is being compiled. For each of them, it writes a Java
 * class that computes the same result directly. In EO, an object whose body
 * is written in Java is called an "atom", so this module writes atoms. The
 * body of the original object is then replaced by a call to the new atom,
 * and the many small objects are never built.</p>
 *
 * <p>This module does not understand EO by itself. The math of EO is known
 * only by an external program called {@code phino}. This module runs phino
 * through the class {@code Phino}, and it accepts only
 * the one version of phino that it was tested with. The module gives phino
 * all the objects of the program together in one file, which is called the
 * "world". This is necessary because an object in one file often uses
 * objects from other files. Every object that may become an atom is called
 * an "entry", and phino is run once for every entry. While phino works on
 * an entry, it writes down every step it takes into a file, which is called
 * the "protocol" of the entry. The Java atom is made from this protocol.
 * When phino cannot compute an entry to the end, the entry is called a
 * "taint", and its object simply stays in EO as it was written.</p>
 *
 * <p>The work happens in stages, one after another. Every stage is a
 * {@link org.cactoos.Proc} that works in the home directory of the lowering, and
 * the class {@link org.eolang.lowering.Lowering} runs them in this
 * order:</p>
 *
 * <ol>
 * <li>{@code Pruning} removes the tests from copies of
 * the sources;</li>
 * <li>{@code Planting} writes down the entries;</li>
 * <li>{@code Merging} puts everything into the
 * world;</li>
 * <li>{@code Morphing} runs phino once for every
 * entry;</li>
 * <li>{@code Rendering} writes the Java atoms;</li>
 * <li>{@code Patching} puts the atoms into the EO
 * objects.</li>
 * </ol>
 *
 * <p>No stage skips any work and no stage tries again after a failure. If
 * something goes wrong, the whole build fails. This way, a build never
 * quietly produces something that nobody can explain.</p>
 *
 * @since 0.64.0
 * @see <a href="https://www.eolang.org">Project site www.eolang.org</a>
 * @see <a href="https://github.com/objectionary/eo">GitHub project</a>
 */
package org.eolang.lowering;
