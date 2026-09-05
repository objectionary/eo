/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.List;
import java.util.Optional;

/**
 * A value known at build time, standing in the reduction tree.
 *
 * <p>It comes from a literal of the XMIR fragment, or from the result of
 * a fired atom folded back into the tree. Either way it is a forma and
 * the bytes it carries, rendered as the same carrier application the
 * compiler itself writes, with the positional {@code α0} binding names
 * that phino resolves on dispatch. A number and a string wrap their
 * datum in a bytes carrier of their own, since that is the one void
 * each of them declares.</p>
 *
 * @since 0.76.0
 */
public final class Literal implements Term {

    /**
     * The forma of the value.
     */
    private final String forma;

    /**
     * The bytes, as dash-joined hex pairs.
     */
    private final String hex;

    /**
     * Ctor.
     * @param carrier The forma of the value
     * @param bytes The bytes, as dash-joined hex pairs
     */
    public Literal(final String carrier, final String bytes) {
        this.forma = carrier;
        this.hex = bytes;
    }

    @Override
    public String phi() {
        final String out;
        if ("number".equals(this.forma) || "string".equals(this.forma)) {
            out = String.format(
                "Φ.%s(α0 ↦ Φ.bytes(α0 ↦ ⟦ Δ ⤍ %s ⟧))", this.forma, this.hex
            );
        } else if ("bytes".equals(this.forma)) {
            out = String.format("Φ.bytes(α0 ↦ ⟦ Δ ⤍ %s ⟧)", this.hex);
        } else if ("bool".equals(this.forma)) {
            if ("FF-".equals(this.hex)) {
                out = "Φ.true";
            } else {
                out = "Φ.false";
            }
        } else {
            throw new IllegalStateException(
                String.format("No carrier application renders the forma '%s'", this.forma)
            );
        }
        return out;
    }

    @Override
    public String key() {
        return String.format("%s:%s", this.forma, this.hex);
    }

    @Override
    public String forma() {
        return this.forma;
    }

    @Override
    public boolean matches(final Shape shape) {
        return false;
    }

    @Override
    public Optional<List<Binding>> arguments(final Shape shape) {
        return Optional.empty();
    }

    @Override
    public Optional<List<Term>> again() {
        return Optional.empty();
    }

    @Override
    public Term swapped(final Shape shape, final Term swap) {
        return this;
    }
}
