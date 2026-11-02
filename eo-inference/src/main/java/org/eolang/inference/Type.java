/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import org.xembly.Directives;

/**
 * What a head of the program states before inference runs.
 *
 * <p>A head is a reference to another object, a datum, or a termination.
 * Each kind writes its own element inside the row that {@link Starts}
 * puts into the {@code heads} section of {@code facts.xml}. These are
 * facts of the source, not the per-copy answers the engine works out.</p>
 *
 * <p>What comes back leaves the cursor where it found it, since a row holds
 * one type and the row that follows starts beside it.</p>
 *
 * <p>A reference names its target by locator; a datum and a termination
 * name no object. The locator is the one the source establishes, before
 * any calls or reads are resolved by the engine.</p>
 *
 * @since 0.69.0
 */
@FunctionalInterface
interface Type {

    /**
     * This type as the contents of a row.
     *
     * @return The directives
     */
    Directives directives();

    /**
     * The object of the program this type names.
     *
     * @return The locator, empty where the type names no one object
     */
    default String names() {
        return "";
    }
}
