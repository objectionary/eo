/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * A subclass overriding add() to record every attribute name it is asked
 * to add, proving the method is overridable (#6273).
 *
 * @since 0.1.0
 */
final class Observant extends PhDefault {

    /**
     * Names seen so far, in the order add() was called.
     */
    private final List<String> names;

    /**
     * Ctor.
     */
    Observant() {
        super(new Attrs(new Attr("x", new AtVoid("x"))));
        this.names = new ArrayList<>(0);
    }

    @Override
    public void add(final String name, final Attribute attr) {
        this.names.add(name);
        super.add(name, attr);
    }

    /**
     * Attribute names seen so far.
     *
     * @return Names, in call order
     */
    List<String> seen() {
        return Collections.unmodifiableList(this.names);
    }
}
