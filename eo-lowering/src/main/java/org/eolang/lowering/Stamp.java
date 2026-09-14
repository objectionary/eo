/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import org.w3c.dom.Element;

/**
 * The marks of a lowered formation: the digest of its sidecar, its purity
 * and the {@code λ} binding naming the atom of its carrier.
 *
 * <p>A sibling atom appended to a fragment is named after the digest, so
 * that two markers whose programs render the same share one atom.</p>
 *
 * @since 0.77.0
 */
final class Stamp {

    /**
     * The digest of the sidecar.
     */
    private final String digest;

    /**
     * The carrier of the value.
     */
    private final String carrier;

    /**
     * Whether the atom reads nothing but data and enters no formation.
     */
    private final boolean pure;

    /**
     * Ctor.
     *
     * @param hash The digest of the sidecar
     * @param forma The carrier of the value
     * @param clean Whether the atom reads nothing but data and enters
     *  no formation
     */
    Stamp(final String hash, final String forma, final boolean clean) {
        this.digest = hash;
        this.carrier = forma;
        this.pure = clean;
    }

    /**
     * The name of a sibling atom carrying this stamp.
     *
     * @return The name
     */
    String name() {
        return String.format("l🌵%s", this.digest);
    }

    /**
     * Mark a formation.
     *
     * @param formation The formation
     */
    void on(final Element formation) {
        formation.setAttribute("lowered", this.digest);
        if (this.pure) {
            formation.setAttribute("pure", "true");
        }
        final Element lambda = formation.getOwnerDocument().createElement("o");
        lambda.setAttribute("name", "λ");
        lambda.setAttribute("atom", String.format("Φ.%s", this.carrier));
        formation.appendChild(lambda);
    }
}
