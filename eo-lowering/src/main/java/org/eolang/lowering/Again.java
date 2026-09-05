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
 * A call of the formation being lowered to itself, standing in the
 * reduction tree.
 *
 * <p>It holds the arguments of the call, in their positional order, and
 * nothing else: the formation is the fragment itself, so calling it
 * again means computing the same body over the values the arguments
 * settle into. It renders as a formation binding the arguments to
 * {@code a0}, {@code a1} and so on, next to the marker λ
 * {@code L_self}, so that phino parks on it wherever it stands and never
 * looks inside; the arity is whatever the call has, and the universe
 * needs no row for it. It has no key, since
 * it is not a value, and a reduction that finds it at the root of a
 * tree settles that tree into a repeat instead of an answer.</p>
 *
 * @since 0.76.0
 */
public final class Again implements Term {

    /**
     * The arguments of the call, in their positional order.
     */
    private final List<Term> args;

    /**
     * Ctor.
     * @param arguments The arguments of the call, in their positional order
     */
    public Again(final List<Term> arguments) {
        this.args = arguments;
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
    public Optional<List<Term>> again() {
        return Optional.of(Collections.unmodifiableList(this.args));
    }

    @Override
    public Term swapped(final Shape shape, final Term swap) {
        return new Again(
            this.args.stream()
                .map(arg -> arg.swapped(shape, swap))
                .collect(Collectors.toList())
        );
    }
}
