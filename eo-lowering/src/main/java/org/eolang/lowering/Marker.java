/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

/**
 * A value phino goes on with, spelled as the carrier it stands in.
 *
 * <p>A symbol stands in its carrier as a marker, {@code ⟦ λ ⤍ S4 ⟧}: a
 * number is the marker under {@code Φ.bytes} under {@code Φ.number}, bytes
 * are the marker under {@code Φ.bytes} alone, and a bool is a {@code Φ.bool} whose
 * {@code if} is a fork on the symbol, so that every operation of the bool
 * reaches the engine through the one {@code L_fork} atom. A literal stands
 * the same way with a Δ payload, and a bool literal is {@code Φ.true} or
 * {@code Φ.false}. A value of no known carrier is a bare marker.</p>
 *
 * @since 0.76.0
 */
public final class Marker {

    /**
     * The key, {@code sym:S4} or {@code number:HEX}.
     */
    private final String key;

    /**
     * The carrier.
     */
    private final String carrier;

    /**
     * Ctor.
     *
     * @param operand The key, {@code sym:S4} or {@code number:HEX}
     * @param forma The carrier
     */
    public Marker(final String operand, final String forma) {
        this.key = operand;
        this.carrier = forma;
    }

    /**
     * The φ-expression.
     *
     * @return The text
     */
    public String phi() {
        if ("tuple".equals(this.carrier)) {
            throw new IllegalStateException(
                String.format(
                    "The tuple '%s' has parts, which a marker alone cannot spell", this.key
                )
            );
        }
        final String out;
        if ("number".equals(this.carrier) || "string".equals(this.carrier)) {
            out = String.format(
                "Φ.%s( φ ↦ Φ.bytes( φ ↦ ⟦ %s ⟧ ) )", this.carrier, this.payload()
            );
        } else if ("bytes".equals(this.carrier)) {
            out = String.format("Φ.bytes( φ ↦ ⟦ %s ⟧ )", this.payload());
        } else if ("bool".equals(this.carrier)) {
            out = this.truth();
        } else {
            out = String.format("⟦ %s ⟧", this.payload());
        }
        return out;
    }

    private String truth() {
        final String out;
        if (this.key.startsWith("sym:")) {
            out = String.format(
                "Φ.bool( if ↦ ⟦ left ↦ ∅, right ↦ ∅, guard ↦ ⟦ %s ⟧, λ ⤍ L_fork ⟧ )",
                this.payload()
            );
        } else if ("00-".equals(this.value())) {
            out = "Φ.false";
        } else {
            out = "Φ.true";
        }
        return out;
    }

    private String payload() {
        final String out;
        if (this.key.startsWith("sym:")) {
            out = String.format("λ ⤍ %s", this.value());
        } else {
            out = String.format("Δ ⤍ %s", this.value());
        }
        return out;
    }

    private String value() {
        return this.key.substring(this.key.indexOf(':') + 1);
    }
}
