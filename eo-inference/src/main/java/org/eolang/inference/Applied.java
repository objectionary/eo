/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * The locator of the application a call makes.
 *
 * <p>A call that carries arguments is two objects written on one line. The
 * {@code ^.if} of an {@code and} takes the {@code if} of a {@code Φ.bool},
 * which is a void, and applies it to two arguments. What the line comes back
 * as and what those arguments fill the voids of are different objects, and one
 * locator answered for both: a reader after the second found the first, saw
 * that a {@code Φ.bool} declares an {@code if}, and put the comparison written
 * at that call site there, where it stood for every reader of that void in the
 * program (#8552).</p>
 *
 * <p>So the application is named apart, with the brackets a φ-calculus
 * expression writes an application with and nothing inside them: what goes in
 * is in the locators of the arguments already, which hang off the call as
 * {@code α0} and {@code α1}. The name is made here rather than by the parser,
 * since an object of the XMIR carries one locator and this is a second one for
 * the same object.</p>
 *
 * @since 0.73.0
 */
final class Applied {

    /**
     * The locator of the call.
     */
    private final String site;

    /**
     * Ctor.
     *
     * @param call The locator of the call
     */
    Applied(final String call) {
        this.site = call;
    }

    /**
     * The locator of the application this call makes.
     *
     * @return The locator of the call with the brackets of an application on
     *  the end of it
     */
    String made() {
        return this.site.concat("()");
    }
}
