/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

/**
 * The formation a λ name was boxed for.
 *
 * <p>It takes a λ name minted by {@link Lambda} and answers the locator it
 * spells out: {@code L_box_p__stdin__all_dlines} becomes
 * {@code Φ.stdin.all-lines}. This is the way back over the wire, where a
 * fire arrives under a name and the build has to say which formation it
 * entered.</p>
 *
 * @since 0.77.0
 */
final class Place {

    /**
     * The λ name.
     */
    private final String lambda;

    /**
     * Ctor.
     *
     * @param name The λ name
     */
    Place(final String name) {
        this.lambda = name;
    }

    /**
     * The locator.
     *
     * @return The locator, such as {@code Φ.number.twice}
     */
    String name() {
        if (!this.lambda.startsWith(Lambda.PREFIX)) {
            throw new IllegalArgumentException(
                String.format("The name '%s' is not the name of a box", this.lambda)
            );
        }
        return this.read(this.lambda.substring(Lambda.PREFIX.length()));
    }

    private String read(final String spelt) {
        final StringBuilder out = new StringBuilder(spelt.length());
        int idx = 0;
        while (idx < spelt.length()) {
            final char sym = spelt.charAt(idx);
            if (sym == '_') {
                final String marker = this.escape(spelt, idx);
                out.append(this.meant(marker));
                idx += marker.length() + 1;
            } else {
                out.append(sym);
                idx += 1;
            }
        }
        return out.toString();
    }

    private String escape(final String spelt, final int idx) {
        int width = 2;
        if (idx + 1 < spelt.length() && spelt.charAt(idx + 1) == 'u') {
            width = 8;
        }
        if (idx + width > spelt.length()) {
            throw new IllegalArgumentException(
                String.format("The name '%s' breaks off in the middle", this.lambda)
            );
        }
        return spelt.substring(idx + 1, idx + width);
    }

    private String meant(final String marker) {
        final String out;
        if ("_".equals(marker)) {
            out = ".";
        } else if ("d".equals(marker)) {
            out = "-";
        } else if ("p".equals(marker)) {
            out = "Φ";
        } else if ("c".equals(marker)) {
            out = "🌵";
        } else if (marker.charAt(0) == 'u') {
            out = new String(
                Character.toChars(Integer.parseInt(marker.substring(1), 16))
            );
        } else {
            throw new IllegalArgumentException(
                String.format(
                    "The name '%s' spells '_%s', which means nothing", this.lambda, marker
                )
            );
        }
        return out;
    }
}
