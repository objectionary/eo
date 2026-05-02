/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.AbstractMap;

/**
 * A {@link java.util.Map.Entry} of an attribute name to its {@link Attr}.
 *
 * <p>This is just a typed alias for
 * {@link AbstractMap.SimpleEntry SimpleEntry&lt;String, Attr&gt;}, kept short
 * so that subclasses of {@link PhDefault} can write
 * {@code super(new Attrs(new AttrEntry("x", new AtVoid("x"))))} without
 * any method calls in the constructor body.</p>
 *
 * @since 0.59
 */
public final class AttrEntry extends AbstractMap.SimpleEntry<String, Attr> {

    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = -1L;

    /**
     * Ctor.
     * @param name Attribute name
     * @param attr Attribute
     */
    public AttrEntry(final String name, final Attr attr) {
        super(name, attr);
    }
}
