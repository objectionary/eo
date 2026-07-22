/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

/**
 * A φ-calculus term, rendered as text.
 *
 * <p>Every implementation returns the canonical textual representation
 * of how the underlying object was constructed in the φ-calculus, with
 * no instance identity baked in (no hash codes, no allocation order,
 * no source location). Structurally identical constructions must
 * produce equal strings; structurally different constructions must
 * produce different strings.</p>
 *
 * <p>The {@code φTerm()} output is used for debug rendering
 * and structural inspection of an object's φ-form.</p>
 *
 * @since 0.60
 */
@SuppressWarnings("PMD.ImplicitFunctionalInterface")
public interface Term {

    /**
     * To φ-calculus term, as text.
     * @return The expression in φ-calculus
     * @checkstyle MethodNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.MethodNamingConventions")
    String φTerm();
}
