/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

/**
 * Atom.
 *
 * <p>A native object implemented in the language EO is compiled into.
 * For EO end user atoms look like magic box. That's why all atoms have
 * λ function, that calculates the final object.</p>
 *
 * @since 0.36.0
 */
public interface Atom {
    /**
     * Executes λ function and calculates object.
     * @return Object calculated from λ function.
     */
    Phi lambda() throws Exception;
}
