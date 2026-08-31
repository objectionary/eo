/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.function.BiConsumer;

/**
 * Attributes that can be walked past without a map being built for them.
 *
 * <p>{@link java.util.Map#entrySet()} is the portable way to walk a map, and
 * both maps of attributes here build something on every call of it. A caller
 * that only wants to look at each name and attribute once — {@link
 * CopiedAttrs} taking a copy of the empty ones — asks for this instead.</p>
 *
 * @since 0.63
 */
@FunctionalInterface
interface Walkable {

    /**
     * Hand every name and attribute to the action, in the order they arrived.
     * @param action What to do with each name and attribute
     */
    void each(BiConsumer<String, Attribute> action);
}
