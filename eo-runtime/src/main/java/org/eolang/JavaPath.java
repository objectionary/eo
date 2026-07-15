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
 * Since EOLANG allows using dashes in object names, they are converted to
 * underscores for Java.</p>
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
     * @return Java package name
     */
    String pkg() {
        return this.java(true);
    }

    /**
     * Build the Java name, prefixing each EO name segment.
     * @param wrap When true, the last segment is a package too (EO_), otherwise
     *  it is the object itself (EO)
     * @return Java notation of the path
     */
    private String java(final boolean wrap) {
        final String[] parts = JavaPath.PHI.matcher(this.object).replaceAll("").split("\\.");
        final StringBuilder out = new StringBuilder("org.eolang");
        for (int idx = 0; idx < parts.length; ++idx) {
            final String prefix;
            if (!wrap && idx == parts.length - 1) {
                prefix = "EO";
            } else {
                prefix = "EO_";
            }
            out.append('.').append(prefix)
                .append(parts[idx].replace("$", "$EO").replace("-", "_"));
        }
        return out.toString();
    }
}
