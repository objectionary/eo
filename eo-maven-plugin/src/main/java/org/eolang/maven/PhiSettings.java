/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

/**
 * The settings that change what the transpiler writes into the generated
 * Java: whether located objects get wrapped into {@code PhCoverage}, and
 * which class a generated class extends instead of {@code PhDefault}.
 * Both are read only by {@code to-java.xsl} and are folded into the
 * transpile cache key, so they travel together.
 * @since 0.62.0
 */
final class PhiSettings {

    /**
     * Whether located objects are wrapped into {@code PhCoverage}.
     */
    private final boolean coverage;

    /**
     * The class a generated class extends instead of {@code PhDefault}.
     */
    private final String superclass;

    /**
     * Ctor.
     * @param coverage Whether located objects are wrapped into {@code PhCoverage}
     * @param superclass The class a generated class extends instead of {@code PhDefault}
     */
    PhiSettings(final boolean coverage, final String superclass) {
        this.coverage = coverage;
        this.superclass = superclass;
    }

    /**
     * Whether located objects are wrapped into {@code PhCoverage}.
     * @return True when they are
     */
    boolean coverage() {
        return this.coverage;
    }

    /**
     * The class a generated class extends instead of {@code PhDefault}.
     * @return The class name
     */
    String superclass() {
        return this.superclass;
    }
}
