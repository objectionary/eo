/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.AbstractMap;

/**
 * A {@link java.util.Map.Entry} of an attribute name to its {@link Attribute}.
 *
 * <p>This is just a typed alias for
 * {@link AbstractMap.SimpleImmutableEntry SimpleImmutableEntry&lt;String,
 * Attr&gt;}, kept short so that subclasses of {@link PhDefault} can write
 * {@code super(new Attrs(new Attr("x", new AtVoid("x"))))} without
 * any method calls in the constructor body. The entry is immutable, so an
 * {@link Attrs} it was handed to keeps what it was given, no matter what the
 * caller does with its own copy of the entry afterwards.</p>
 *
 * @since 0.59
 */
public final class Attr extends AbstractMap.SimpleImmutableEntry<String, Attribute> {

    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = -1L;

    /**
     * Ctor.
     *
     * @param name Attribute name
     * @param attr Attribute
     */
    public Attr(final String name, final Attribute attr) {
        super(name, attr);
    }
}
