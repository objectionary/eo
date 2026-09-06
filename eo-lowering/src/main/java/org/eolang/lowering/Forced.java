/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.List;
import java.util.Optional;

/**
 * The bytes a term is forced into: what {@code as-bytes} answers.
 *
 * <p>A number, a string and a bool carry their bytes in the one slot
 * their carrier binds, so {@code as-bytes} on any of them resolves
 * structurally in the {@link Universe}, with no atom firing and no
 * record written, and the view stands in the tree as the term it wraps,
 * keyed by the same value once that value is known: a symbol keeps its
 * key, since a record names a symbolic carrier the same way whatever it
 * is wrapped in, and a literal changes its forma to bytes. Bytes have no
 * {@code as-bytes} of their own, so the view of bytes renders as the
 * bytes themselves. A const is such a view: the parser turns {@code x!}
 * into {@code (dataized x).as-bytes}, and forcing is already what a
 * protocol does, since every step of it is a Java local computed once,
 * in order, so the view is all that is left of the wrapper.</p>
 *
 * <p>A fragment may settle into such a view, and then the answer is the
 * bytes of a local the step declared a double. {@link Rendering} hands
 * that answer over through the raw bits of the local, the same reading
 * {@code L_bytes_eq} takes of a number, so the atom answers bytes where
 * the formation did.</p>
 *
 * @since 0.76.0
 */
public final class Forced implements Term {

    /**
     * The term whose bytes this is.
     */
    private final Term inner;

    /**
     * Ctor.
     * @param term The term whose bytes this is
     */
    public Forced(final Term term) {
        this.inner = term;
    }

    @Override
    public String phi() {
        final String out;
        if ("bytes".equals(this.inner.forma())) {
            out = this.inner.phi();
        } else {
            out = String.format("%s.as-bytes", this.inner.phi());
        }
        return out;
    }

    @Override
    public String key() {
        final String key = this.inner.key();
        final String out;
        if (key.isEmpty() || key.startsWith("sym:")) {
            out = key;
        } else {
            out = String.format("bytes:%s", key.split(":", 2)[1]);
        }
        return out;
    }

    @Override
    public String forma() {
        final String out;
        if (this.inner.key().isEmpty()) {
            out = "";
        } else {
            out = "bytes";
        }
        return out;
    }

    @Override
    public boolean matches(final Shape shape) {
        return this.inner.matches(shape);
    }

    @Override
    public Optional<List<Binding>> arguments(final Shape shape) {
        return this.inner.arguments(shape);
    }

    @Override
    public Optional<Again> again() {
        return Optional.empty();
    }

    @Override
    public Term swapped(final Shape shape, final Term swap) {
        return new Forced(this.inner.swapped(shape, swap));
    }
}
