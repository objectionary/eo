/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.regex.Pattern;

/**
 * Java path.
 *
 * <p>The class converts object path in eolang notation to java notation.
 * The terminal segment (the object itself) is prefixed with {@code EO},
 * while every package segment before it is prefixed with {@code EO_}, so
 * that a package can never clash with an object class of the same name.
 * For example
 * - "Φ.number" -> "org.eolang.EOnumber"
 * - "Φ.number.power" -> "org.eolang.EO_number.EOpower"
 * - "Φ.io.stdout$bytes" -> "org.eolang.EO_io.EOstdout$EObytes"
 * Every segment is escaped the way {@code eo:clean} escapes it in
 * {@code _java-names.xsl}, which the transpiler uses to name the class it
 * writes, so that a name the transpiler accepted is found at runtime. The
 * dollar is escaped ahead of everything else, so that an escape is never
 * read as a dollar the name itself carried; the dash then becomes an
 * underscore and the underscore an escape of its own, which no dash can
 * produce, so that "a-_b" and "a_-b" stay two names; and every character
 * Java forbids in an identifier becomes an escape of six digits, not four,
 * since four drop everything above the sixteenth bit.</p>
 *
 * @since 0.29
 */
final class JavaPath {

    /**
     * Phi pattern.
     */
    private static final Pattern PHI = Pattern.compile("^Φ\\.?");

    /**
     * Object name in eolang notation.
     */
    private final String object;

    /**
     * Ctor.
     *
     * @param obj Object name
     */
    JavaPath(final String obj) {
        this.object = obj;
    }

    @Override
    public String toString() {
        return this.java(false);
    }

    /**
     * The Java name of the package that hosts this object, i.e. with every
     * segment (including the last one) treated as a package. Used to locate
     * the {@code package-info} of the object's own package.
     *
     * @return Java package name
     */
    String pkg() {
        return this.java(true);
    }

    private String java(final boolean wrap) {
        final String[] parts = JavaPath.PHI.matcher(this.object).replaceAll("").split("\\.", -1);
        final StringBuilder out = new StringBuilder("org.eolang");
        for (int idx = 0; idx < parts.length; ++idx) {
            final String prefix;
            if (!wrap && idx == parts.length - 1) {
                prefix = "EO";
            } else {
                prefix = "EO_";
            }
            out.append('.').append(prefix).append(JavaPath.clean(parts[idx]));
        }
        return out.toString();
    }

    private static String clean(final String name) {
        final String swapped = name
            .replace("$", "$EO")
            .replace("_", JavaPath.escaped('_'))
            .replace('-', '_')
            .replace('@', 'φ')
            .replace("α", "_");
        final StringBuilder out = new StringBuilder(swapped.length());
        int idx = 0;
        while (idx < swapped.length()) {
            final int code = swapped.codePointAt(idx);
            if (Character.isLetterOrDigit(code) || code == '_' || code == '$') {
                out.appendCodePoint(code);
            } else {
                out.append(JavaPath.escaped(code));
            }
            idx += Character.charCount(code);
        }
        return out.toString();
    }

    private static String escaped(final int code) {
        return String.format("$u%06X", code);
    }
}
