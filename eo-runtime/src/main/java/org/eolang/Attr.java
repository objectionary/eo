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
 * {@code super(new Attrs(new Attr("x")))} without any method calls in the
 * constructor body. The entry is immutable, so an {@link Attrs} it was
 * handed to keeps what it was given, no matter what the caller does with
 * its own copy of the entry afterwards.</p>
 *
 * @since 0.59
 */
public final class Attr extends AbstractMap.SimpleImmutableEntry<String, Attribute> {

    /**
     * Serialization identifier.
     */
    private static final long serialVersionUID = -1L;

    /**
     * Ctor that declares a void attribute.
     *
     * <p>The void is kept under the name {@link #veiled(String)} gives
     * it. Other objects can fill it, and only its own object can read it,
     * through {@link PhDefault#free(String)}.</p>
     *
     * @param name Name of the void attribute, as the EO source spells it
     */
    public Attr(final String name) {
        this(Attr.veiled(name), new AtVoid(name));
    }

    /**
     * Ctor.
     *
     * @param name Attribute name
     * @param attr Attribute
     */
    public Attr(final String name, final Attribute attr) {
        super(name, attr);
    }

    /**
     * The name a void is kept under.
     *
     * <p>The parser puts a mark in front of the name of every void (#9788).
     * Other objects can still fill the void, by position or by name, but
     * they cannot read it. The {@code φ} and {@code ρ} voids keep their
     * names, as the parser leaves them alone too. This is the only place in
     * the runtime that knows the mark.</p>
     *
     * @param name The name of the void, as the EO source spells it
     * @return The name the void is kept under
     */
    static String veiled(final String name) {
        final String hidden;
        if (Phi.PHI.equals(name) || Phi.RHO.equals(name)) {
            hidden = name;
        } else {
            hidden = "v🌵".concat(name);
        }
        return hidden;
    }
}
