/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * Type inference for XMIR.
 *
 * <p>The checker looks for one kind of mistake: an attribute is taken
 * from an object that certainly doesn't have it. In this program, for
 * example, {@code t} does have {@code next}, but the object attached to
 * {@code next} has no {@code foo}:</p>
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
 * is taken from must have that attribute. An application files a
 * pending check: this argument must fit into that void.</p>
 *
 * <p>Then the checks are drained one by one, each of them either
 * deciding, splitting into smaller checks, or waiting for facts that may
 * never come. A mistake is reported only when the object that misses an
 * attribute is complete, so that parts of the program this module cannot
 * see — atoms, other files, delegation through {@code φ} — make it
 * silent rather than wrong.</p>
 *
 * <p>Only the first of those rules is implemented so far, together with
 * the preparation the rest of them will need. The entry point is
 * {@link org.eolang.inference.Inference}.</p>
 *
 * @since 0.67.0
 * @see <a href="https://www.eolang.org">Project site www.eolang.org</a>
 * @see <a href="https://github.com/objectionary/eo">GitHub project</a>
 */
package org.eolang.inference;
