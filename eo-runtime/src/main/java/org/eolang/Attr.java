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
 * {@link AbstractMap.SimpleEntry SimpleEntry&lt;String, Attr&gt;}, kept short
 * so that subclasses of {@link PhDefault} can write
 * {@code super(new Attrs(new Attr("x", new AtVoid("x"))))} without
 * any method calls in the constructor body.</p>
 *
 * @since 0.59
 */
public final class Attr extends AbstractMap.SimpleEntry<String, Attribute> {

    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = -1L;

    /**
     * Ctor.
     * @param name Attribute name
     * @param attr Attribute
     */
    public Attr(final String name, final Attribute attr) {
        super(name, attr);
    }
}
