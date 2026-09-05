/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.List;
import java.util.Optional;

/**
 * A value not known until run time, standing in the reduction tree.
 *
 * <p>It stands for a void of the fragment, or for a step already minted
 * from a parked record. It renders as an application of the carrier its
 * forma names, holding a marker formation in the data slot: dispatch
 * finds the methods of the carrier as usual, and the marker parks the
 * atom that finally demands the bytes, which is how the next record
 * points here. A number, a string and a bool wrap the marker in a
 * bytes carrier of their own, since that is the one void each of them
 * declares.</p>
 *
 * @since 0.76.0
 */
public final class Symbol implements Term {

    /**
     * The name, such as {@code v0} for a void or {@code s1} for a step.
     */
    private final String name;

    /**
     * The forma of the value.
     */
    private final String forma;

    /**
     * Ctor.
     * @param label The name, such as {@code v0} or {@code s1}
     * @param carrier The forma of the value
     */
    public Symbol(final String label, final String carrier) {
        this.name = label;
        this.forma = carrier;
    }

    @Override
    public String phi() {
        final String out;
        if ("number".equals(this.forma) || "string".equals(this.forma)
            || "bool".equals(this.forma)) {
            out = String.format(
                "Φ.%s(α0 ↦ Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_%s ⟧))", this.forma, this.name
            );
        } else if ("bytes".equals(this.forma)) {
            out = String.format("Φ.bytes(α0 ↦ ⟦ λ ⤍ Sym_%s ⟧)", this.name);
        } else {
            throw new IllegalStateException(
                String.format(
                    "A value of forma '%s' has no symbolic carrier, so '%s' cannot stand mid-tree",
                    this.forma, this.name
                )
            );
        }
        return out;
    }

    @Override
    public String forma() {
        return this.forma;
    }

    @Override
    public String key() {
        return String.format("sym:%s", this.name);
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
    public Optional<Again> again() {
        return Optional.empty();
    }

    @Override
    public Term swapped(final Shape shape, final Term swap) {
        return this;
    }
}
