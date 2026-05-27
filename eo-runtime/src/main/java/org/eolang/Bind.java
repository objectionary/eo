/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * A single attribute binding inside an application: a position or a name
 * paired with the object being bound.
 *
 * <p>It is passed to {@link PhApplication} so that several bindings can be
 * applied to one object at once, instead of nesting an application per
 * binding. The binding knows how to attach itself to a target object and
 * how to render its own φ-term fragment.</p>
 *
 * @since 0.60
 */
public final class Bind {

    /**
     * Attaches the bound object to a target.
     */
    private final Consumer<Phi> command;

    /**
     * Renders the φ-term fragment of this binding.
     */
    private final Supplier<String> term;

    /**
     * Ctor.
     * @param pos The position
     * @param obj The object to bind
     */
    public Bind(final int pos, final Phi obj) {
        this(
            target -> target.put(pos, obj),
            () -> String.format("%d->%s", pos, obj.φTerm())
        );
    }

    /**
     * Ctor.
     * @param name The name of the attribute
     * @param obj The object to bind
     */
    public Bind(final String name, final Phi obj) {
        this(
            target -> target.put(name, obj),
            () -> String.format("%s->%s", name, obj.φTerm())
        );
    }

    /**
     * Ctor.
     * @param command Attaches the bound object to a target
     * @param term Renders the φ-term fragment
     */
    private Bind(final Consumer<Phi> command, final Supplier<String> term) {
        this.command = command;
        this.term = term;
    }

    /**
     * Attach this binding to the given object.
     * @param phi The object to bind into
     */
    void attach(final Phi phi) {
        this.command.accept(phi);
    }

    /**
     * The φ-term fragment of this binding, like {@code 0->x} or {@code name->y}.
     * @return The fragment
     * @checkstyle MethodNameCheck (5 lines)
     */
    @SuppressWarnings("PMD.MethodNamingConventions")
    String φTerm() {
        return this.term.get();
    }
}
