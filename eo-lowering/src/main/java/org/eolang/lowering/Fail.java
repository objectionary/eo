/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.List;
import java.util.Optional;

/**
 * The terminator of the fragment, standing in the reduction tree with
 * the reason it carries.
 *
 * <p>It is what {@code T "reason"} parses into: the one object of EO
 * that has no value, since dataizing it aborts with its reason as the
 * message. It renders as a formation binding the reason to {@code r},
 * next to the marker λ {@code L_fail}, so that phino parks on it
 * wherever it stands and never looks inside; the universe needs no row
 * for it. It has no key and no forma, since it is not a value, and a
 * reduction that finds it at the root of a tree settles that tree into
 * a failure instead of an answer, with the reason reduced into a value
 * of its own: the Java of such a path throws, so an arm that fails has
 * no opinion about what its fork answers, the way an arm that repeats
 * has none. Anywhere else it is refused, since an operation awaiting
 * its value never gets one.</p>
 *
 * @since 0.76.0
 */
public final class Fail implements Term {

    /**
     * The reason of the failure.
     */
    private final Term cause;

    /**
     * Ctor.
     *
     * @param reason The reason of the failure
     */
    public Fail(final Term reason) {
        this.cause = reason;
    }

    /**
     * The reason of the failure.
     *
     * @return The term the message is dataized from
     */
    public Term reason() {
        return this.cause;
    }

    @Override
    public String phi() {
        return String.format("⟦ r ↦ %s, λ ⤍ L_fail ⟧", this.cause.phi());
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
        return this.cause.matches(shape);
    }

    @Override
    public Optional<List<Binding>> arguments(final Shape shape) {
        return this.cause.arguments(shape);
    }

    @Override
    public Optional<Again> again() {
        return Optional.empty();
    }

    @Override
    public Optional<Fail> terminator() {
        return Optional.of(this);
    }

    @Override
    public Term swapped(final Shape shape, final Term swap) {
        return new Fail(this.cause.swapped(shape, swap));
    }
}
