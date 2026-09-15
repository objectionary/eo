/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

/**
 * The λ name a formation is boxed under.
 *
 * <p>It takes the locator of a formation and answers the name, spelling
 * the locator out letter for letter: {@code Φ.stdin.all-lines} becomes
 * {@code L_box_p__stdin__all_dlines}. Nothing is looked up, so one
 * formation carries one name in every build, and {@link Place} reads the
 * locator back out of the name. Only a lower-case letter, a digit and an
 * underscore may stand in a λ name, which is why a dot is spelt
 * {@code __}, a dash {@code _d}, the root {@code _p}, the mark of a test
 * {@code _c}, and anything else its code point in hex after {@code _u}.</p>
 *
 * @since 0.77.0
 */
final class Lambda {

    /**
     * The head of every name.
     */
    static final String PREFIX = "L_box";

    /**
     * The root of every locator.
     */
    private static final int PHI = "Φ".codePointAt(0);

    /**
     * The mark the parser puts in the name of a test.
     */
    private static final int CACTUS = "🌵".codePointAt(0);

    /**
     * The locator.
     */
    private final String place;

    /**
     * Ctor.
     *
     * @param locator The locator of the formation
     */
    Lambda(final String locator) {
        this.place = locator;
    }

    /**
     * The name.
     *
     * @return The λ name, such as {@code L_box_p__number__twice}
     */
    String name() {
        final StringBuilder out = new StringBuilder(Lambda.PREFIX);
        this.place.codePoints().forEach(code -> out.append(Lambda.spelt(code)));
        return out.toString();
    }

    private static String spelt(final int code) {
        final String out;
        if (code >= 'a' && code <= 'z' || code >= '0' && code <= '9') {
            out = new String(Character.toChars(code));
        } else if (code == '.') {
            out = "__";
        } else if (code == '-') {
            out = "_d";
        } else if (code == Lambda.PHI) {
            out = "_p";
        } else if (code == Lambda.CACTUS) {
            out = "_c";
        } else {
            out = String.format("_u%06x", code);
        }
        return out;
    }
}
