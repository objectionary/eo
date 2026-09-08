/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.StringJoiner;

/**
 * One application of the fragment, standing in the reduction tree.
 *
 * <p>It is a method dispatched on a receiver with named arguments, and
 * it is the only kind of node a reduction can rewrite: when a record
 * proves what this site evaluates to, the site gives way to a literal or
 * to the symbol of a new step. Until then it has no key, since its value
 * is exactly what is not known yet. It renders as the dispatch it is
 * while the receiver is not settled, or while the universe answers the
 * method for the forma of the receiver, so that phino evaluates what it
 * can. Once the receiver is settled and the method is one the universe
 * does not model — an EO object of the runtime, such as {@code minus}
 * or {@code as-fixed} — it renders as a marker formation instead: the
 * receiver under {@code self}, the method and the forma the tables of
 * {@code eo:inference} witness for the value as data, the arguments as
 * {@code a0}, {@code a1} and so on, next to the λ {@code L_dispatch},
 * so that phino parks on it when it demands the value and the record
 * carries everything a {@link Dispatch} step needs.</p>
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
     * The forma the tables witness for the value, or empty.
     */
    private final String witnessed;

    /**
     * Ctor, for a site whose value the tables say nothing about.
     *
     * @param verb The method to dispatch
     * @param self The receiver of the dispatch
     * @param arguments The arguments of the application
     */
    public Site(final String verb, final Term self, final List<Binding> arguments) {
        this(verb, self, arguments, "");
    }

    /**
     * Ctor.
     *
     * @param verb The method to dispatch
     * @param self The receiver of the dispatch
     * @param arguments The arguments of the application
     * @param forma The forma the tables witness for the value, or empty
     */
    public Site(final String verb, final Term self, final List<Binding> arguments,
        final String forma) {
        this.method = verb;
        this.receiver = self;
        this.args = arguments;
        this.witnessed = forma;
    }

    @Override
    public String phi() {
        final String out;
        if (this.receiver.key().isEmpty()
            || new Offered(this.receiver.forma()).has(this.method)) {
            out = this.plain();
        } else {
            out = this.marked();
        }
        return out;
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
    public Optional<Again> again() {
        return Optional.empty();
    }

    @Override
    public Optional<Fail> terminator() {
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
                this.method, this.receiver.swapped(shape, swap), inner, this.witnessed
            );
        }
        return out;
    }

    private String plain() {
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

    private String marked() {
        final Collection<String> parts = new ArrayList<>(this.args.size() + 4);
        parts.add(String.format("self ↦ %s", this.receiver.phi()));
        parts.add(String.format("m🌵 ↦ ⟦ Δ ⤍ %s ⟧", Site.hex(this.method)));
        if (!this.witnessed.isEmpty()) {
            parts.add(String.format("f🌵 ↦ ⟦ Δ ⤍ %s ⟧", Site.hex(this.witnessed)));
        }
        for (int idx = 0; idx < this.args.size(); ++idx) {
            parts.add(String.format("a%d ↦ %s", idx, this.args.get(idx).value().phi()));
        }
        parts.add("λ ⤍ L_dispatch");
        return String.format("⟦ %s ⟧", String.join(", ", parts));
    }

    private static String hex(final String text) {
        final byte[] bytes = text.getBytes(StandardCharsets.UTF_8);
        final StringJoiner out = new StringJoiner("-");
        for (final byte item : bytes) {
            out.add(String.format("%02X", item));
        }
        String joined = out.toString();
        if (bytes.length == 1) {
            joined = String.format("%s-", joined);
        }
        return joined;
    }
}
