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
import java.util.stream.Collectors;

/**
 * A call of the formation being lowered to itself, or of one of its
 * recursive helpers, standing in the reduction tree.
 *
 * <p>It holds the name of the body it resumes, empty for the formation
 * itself, and the arguments of the call in their positional order, and
 * nothing else: resuming a body means computing that body again over
 * the values the arguments settle into. It renders as a formation
 * binding the arguments to {@code a0}, {@code a1} and so on, next to
 * the marker λ {@code L_self}, so that phino parks on it wherever it
 * stands and never looks inside; the arity is whatever the call has,
 * and the universe needs no row for it. It has no key, since it is not
 * a value, and a reduction that finds it at the root of a tree settles
 * that tree into a repeat instead of an answer.</p>
 *
 * @since 0.76.0
 */
public final class Again implements Term {

    /**
     * The name of the body the call resumes, empty for the formation.
     */
    private final String target;

    /**
     * The arguments of the call, in their positional order.
     */
    private final List<Term> args;

    /**
     * Ctor, for the call of the formation to itself.
     * @param arguments The arguments of the call, in their positional order
     */
    public Again(final List<Term> arguments) {
        this("", arguments);
    }

    /**
     * Ctor.
     * @param name The name of the body the call resumes, empty for the
     *  formation itself
     * @param arguments The arguments of the call, in their positional order
     */
    public Again(final String name, final List<Term> arguments) {
        this.target = name;
        this.args = arguments;
    }

    /**
     * The name of the body the call resumes.
     * @return The name of the helper, empty for the formation itself
     */
    public String name() {
        return this.target;
    }

    /**
     * The arguments of the call.
     * @return The arguments, in their positional order
     */
    public List<Term> arguments() {
        return Collections.unmodifiableList(this.args);
    }

    @Override
    public String phi() {
        final Collection<String> parts = new ArrayList<>(this.args.size() + 1);
        for (int idx = 0; idx < this.args.size(); ++idx) {
            parts.add(String.format("a%d ↦ %s", idx, this.args.get(idx).phi()));
        }
        parts.add("λ ⤍ L_self");
        return String.format("⟦ %s ⟧", String.join(", ", parts));
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
        return this.args.stream().anyMatch(arg -> arg.matches(shape));
    }

    @Override
    public Optional<List<Binding>> arguments(final Shape shape) {
        Optional<List<Binding>> out = Optional.empty();
        for (int idx = 0; !out.isPresent() && idx < this.args.size(); ++idx) {
            out = this.args.get(idx).arguments(shape);
        }
        return out;
    }

    @Override
    public Optional<Again> again() {
        return Optional.of(this);
    }

    @Override
    public Term swapped(final Shape shape, final Term swap) {
        return new Again(
            this.target,
            this.args.stream()
                .map(arg -> arg.swapped(shape, swap))
                .collect(Collectors.toList())
        );
    }
}
