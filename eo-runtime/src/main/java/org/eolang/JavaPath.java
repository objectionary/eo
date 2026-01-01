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
 * For example
 * - "Φ.org.eolang" -> "EOorg.EOeolang"
 * - "Φ.org.eolang.as-bytes" -> "EOorg.EOeolang.EOas_bytes"
 * - "Φ.org.eolang.as-bytes$bytes" -> "EOorg.EOeolang.EOas_bytes$EObytes"
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
     * Dots pattern.
     */
    private static final Pattern DOTS = Pattern.compile("(^|\\.)([^.]+)");

    /**
     * Object name in eolang notation.
     */
    private final String object;

    /**
     * Ctor.
     * @param obj Object name.
     */
    JavaPath(final String obj) {
        this.object = obj;
    }

    @Override
    public String toString() {
        return DOTS.matcher(JavaPath.PHI.matcher(this.object).replaceAll(""))
            .replaceAll("$1EO$2")
            .replace("$", "$EO")
            .replace("-", "_");
    }
}
