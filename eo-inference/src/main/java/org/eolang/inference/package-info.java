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
 * <p>Nothing here decides whether the program is wrong. Reading the tables
 * together, draining the checks an application files and reporting what is
 * certainly missing was written and taken out again in #6661, because it
 * reported nothing: a verdict needs the object that misses an attribute to
 * have been seen whole, and on a runtime build 48 objects out of 2,505
 * have. The tables have to describe the program before anything can judge
 * it, and that is what this module is for until then.</p>
 *
 * <p>Each rule is a {@link org.eolang.inference.Clue}, which reads the
 * XMIR of a whole program from one directory and writes what it has
 * understood into another. The Maven plugin prepares that XMIR and follows
 * every clue we know through {@link org.eolang.inference.Clues}. Nothing is
 * renamed and nothing fails the build.</p>
 *
 * @since 0.67.0
 * @see <a href="https://www.eolang.org">Project site www.eolang.org</a>
 * @see <a href="https://github.com/objectionary/eo">GitHub project</a>
 */
package org.eolang.inference;
