/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
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
 * in order, so the view is all that is left of the wrapper. Over an
 * object that is no datum — an element a tuple answers — {@code as-bytes}
 * is not a view but the one operation such an object takes, its
 * dataization, so there the term stands unsettled like a site until the
 * atom of the universe parks on it and a step takes its place.</p>
 *
 * @since 0.76.0
 * @todo #8407:30min Let a fragment answer the bytes of a number. The view
 *  of a symbol keeps the key of the symbol, so a protocol settling into
 *  it hands {@code Data.ToPhi} the Java local as it is, a double where
 *  the formation answered bytes, and {@link Reduction} refuses such an
 *  answer for now, wherever a tree settles. Render the view through the
 *  raw bits of the local instead, the way {@code L_bytes_eq} already
 *  compares two numbers, and let the answer carry bytes.
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
        if (this.viewed() && key.startsWith("sym:")) {
            out = key;
        } else if (this.viewed()) {
            out = String.format("bytes:%s", key.split(":", 2)[1]);
        } else {
            out = "";
        }
        return out;
    }

    @Override
    public String forma() {
        final String out;
        if (this.viewed()) {
            out = "bytes";
        } else {
            out = "";
        }
        return out;
    }

    @Override
    public boolean matches(final Shape shape) {
        return this.covered(shape) || this.inner.matches(shape);
    }

    @Override
    public Optional<List<Binding>> arguments(final Shape shape) {
        final Optional<List<Binding>> out;
        if (this.covered(shape)) {
            out = Optional.of(Collections.emptyList());
        } else {
            out = this.inner.arguments(shape);
        }
        return out;
    }

    @Override
    public Optional<Again> again() {
        return Optional.empty();
    }

    @Override
    public Term swapped(final Shape shape, final Term swap) {
        final Term out;
        if (this.covered(shape)) {
            out = swap;
        } else {
            out = new Forced(this.inner.swapped(shape, swap));
        }
        return out;
    }

    private boolean viewed() {
        return Forced.datum(this.inner.forma());
    }

    private boolean covered(final Shape shape) {
        return shape.covers("as-bytes", this.inner.key(), Collections.emptyList());
    }

    private static boolean datum(final String forma) {
        return "number".equals(forma) || "string".equals(forma)
            || "bool".equals(forma) || "bytes".equals(forma);
    }
}
