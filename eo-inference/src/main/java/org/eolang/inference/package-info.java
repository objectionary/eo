/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

/**
 * Type inference for XMIR.
 *
 * <p>This module reads what a program says about its own objects. In this
 * one, for example, {@code t} has {@code next}, the object attached to
 * {@code next} has nothing, and {@code inc} reads {@code foo} off whatever
 * it is given:</p>
 *
 * <pre> [] &gt; app
 *   inc t &gt; @
 *   [] &gt; t
 *     [] &gt; next
 *   [x] &gt; inc
 *     x.next.foo &gt; @</pre>
 *
 * <p>{@link org.eolang.inference.Prepared} makes the XMIR of each file ready:
 * every chain of names is split into one read per name, and the locators
 * are set again. {@link org.eolang.inference.Facts} then reads the prepared
 * XMIR of the whole program and writes {@code facts.xml}: every formation
 * with its attributes, every name read and what it is read off, the head of
 * every chain, and every call with its arguments. Nothing is worked out
 * there, and nothing fails the build.</p>
 *
 * @since 0.67.0
 * @see <a href="https://www.eolang.org">Project site www.eolang.org</a>
 * @see <a href="https://github.com/objectionary/eo">GitHub project</a>
 */
package org.eolang.inference;
