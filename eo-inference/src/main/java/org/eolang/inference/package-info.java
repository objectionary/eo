/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * Type inference for XMIR.
 *
 * <p>This module writes down what a program says about the types of its
 * own objects. In this one, for example, {@code t} has {@code next}, the
 * object attached to {@code next} has nothing, and {@code inc} takes
 * {@code foo} from whatever it is given:</p>
 *
 * <pre> [] &gt; app
 *   inc t &gt; @
 *   [] &gt; t
 *     [] &gt; next
 *   [x] &gt; inc
 *     x.next.foo &gt; @</pre>
 *
 * <p>Every object keeps the type it was born with; nothing is ever
 * renamed or merged. What we learn about types goes into tables
 * instead, and one pass through the XMIR fills them with one rule per
 * kind of object. A formation says what its object provides, and that
 * the list is complete. A reference ({@code ξ.t}) says that one type is
 * a copy of another. A dispatch ({@code .next}) says that the object it
 * is taken from must have that attribute.</p>
 *
 * <p>Then the tables are read together and one kind of mistake is looked
 * for: a name taken from an object that will never have it. A verdict is
 * given only about an object the whole of which has been seen, so that the
 * parts of a program this module cannot see — atoms, delegation through
 * {@code φ}, a void nobody has filled — make it quiet rather than wrong.
 * The mistake of the program above is not among them: {@code foo} is taken
 * from a void and is a mistake for this caller only, while what is reported
 * holds for every caller of the code it is about.</p>
 *
 * <p>Each rule is a {@link org.eolang.inference.Clue}, which reads the
 * XMIR of a whole program from one directory and writes what it has
 * understood into another. The Maven plugin prepares that XMIR and follows
 * every clue we know through {@link org.eolang.inference.Clues}, wrapped in
 * {@link org.eolang.inference.Resolved}, which works out what the dispatches
 * are, and in {@link org.eolang.inference.Concluded}, which reads the tables
 * for mistakes. Nothing is renamed and nothing fails the build.</p>
 *
 * @since 0.67.0
 * @see <a href="https://www.eolang.org">Project site www.eolang.org</a>
 * @see <a href="https://github.com/objectionary/eo">GitHub project</a>
 */
package org.eolang.inference;
