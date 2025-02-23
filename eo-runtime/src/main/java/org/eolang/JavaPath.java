/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

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
        if (!this.object.startsWith("Φ.")) {
            throw new IllegalArgumentException(
                String.format(
                    "Can't build path to .java file from FQN not started from '%s.'",
                    PhPackage.GLOBAL
                )
            );
        }
        return this.object
            .substring(2)
            .replaceAll("(^|\\.)([^.]+)", "$1EO$2")
            .replace("$", "$EO")
            .replace("-", "_");
    }
}
