/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * One application of the fragment, standing in the reduction tree.
 *
 * <p>It is a method dispatched on a receiver with named arguments, and
 * it is the only kind of node a reduction can rewrite: when a record
 * proves what this site evaluates to, the site gives way to a literal or
 * to the symbol of a new step. Until then it has no key, since its value
 * is exactly what is not known yet.</p>
 *
 * @since 0.76.0
 */
public final class Site implements Term {

    /**
     * The method to dispatch.
     */
    private final String method;

    /**
     * The receiver of the dispatch.
     */
    private final Term receiver;

    /**
     * The arguments of the application.
     */
    private final List<Binding> args;

    /**
     * Ctor.
     * @param verb The method to dispatch
     * @param self The receiver of the dispatch
     * @param arguments The arguments of the application
     */
    public Site(final String verb, final Term self, final List<Binding> arguments) {
        this.method = verb;
        this.receiver = self;
        this.args = arguments;
    }

    @Override
    public String phi() {
        String tail = "";
        if (!this.args.isEmpty()) {
            final Collection<String> parts = new ArrayList<>(this.args.size());
            for (final Binding arg : this.args) {
                parts.add(
                    String.format("%s ↦ %s", arg.label(), arg.value().phi())
                );
            }
            tail = String.format("(%s)", String.join(", ", parts));
        }
        return String.format("%s.%s%s", this.receiver.phi(), this.method, tail);
    }

    @Override
    public String key() {
        return "";
    }

    @Override
    public String forma() {
        return "";
    }

    @Override
    public boolean matches(final Shape shape) {
        boolean found = shape.covers(this.method, this.receiver.key(), this.args);
        found = found || this.receiver.matches(shape);
        for (int idx = 0; !found && idx < this.args.size(); ++idx) {
            found = this.args.get(idx).value().matches(shape);
        }
        return found;
    }

    @Override
    public Optional<List<Binding>> arguments(final Shape shape) {
        Optional<List<Binding>> out;
        if (shape.covers(this.method, this.receiver.key(), this.args)) {
            out = Optional.of(Collections.unmodifiableList(this.args));
        } else {
            out = this.receiver.arguments(shape);
            for (int idx = 0; !out.isPresent() && idx < this.args.size(); ++idx) {
                out = this.args.get(idx).value().arguments(shape);
            }
        }
        return out;
    }

    @Override
    public Optional<List<Term>> again() {
        return Optional.empty();
    }

    @Override
    public Term swapped(final Shape shape, final Term swap) {
        final Term out;
        if (shape.covers(this.method, this.receiver.key(), this.args)) {
            out = swap;
        } else {
            final List<Binding> inner = new ArrayList<>(this.args.size());
            for (final Binding arg : this.args) {
                inner.add(
                    new Binding(arg.label(), arg.value().swapped(shape, swap))
                );
            }
            out = new Site(
                this.method, this.receiver.swapped(shape, swap), inner
            );
        }
        return out;
    }
}
