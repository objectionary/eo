/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * Transform {@link Expect} to text a C function can be given.
 *
 * <p>A C string ends at the first NUL byte, so text carrying one cannot
 * cross that boundary intact: the native side would silently read a
 * prefix of what EO handed over. Such text is refused here instead,
 * the way {@link Natural} refuses a size that no syscall could mean.</p>
 *
 * @since 0.57.0
 */
public final class Cstring {

    /**
     * Expect.
     */
    private final Expect<Phi> expect;

    /**
     * Ctor.
     * @param subject What the text is, for the failure message
     * @param phi The object holding the text
     */
    public Cstring(final String subject, final Phi phi) {
        this(new Expect<>(subject, () -> phi));
    }

    /**
     * Ctor.
     * @param expect Expect
     */
    public Cstring(final Expect<Phi> expect) {
        this.expect = expect;
    }

    /**
     * Return it.
     * @return The text
     */
    public String it() {
        return this.expect
            .that(phi -> new Dataized(phi).asString())
            .otherwise("must be a text")
            .must(text -> text.indexOf('\0') < 0)
            .otherwise("must not contain the NUL character, since a C string ends there")
            .it();
    }
}
