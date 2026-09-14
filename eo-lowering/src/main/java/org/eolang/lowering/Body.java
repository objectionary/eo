/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * One body of a program: a formation, or a recursive helper of it.
 *
 * <p>It takes the name of the body, the formas of the voids it runs over,
 * where those voids start in the shared list of locals, and the protocol
 * it computes. It answers all four. A program is a list of these, and the
 * Java of it is one branch per body.</p>
 *
 * @since 0.76.0
 */
final class Body {

    /**
     * The name of the helper, empty for the formation itself.
     */
    private final String label;

    /**
     * The position of the first void of this body among all voids.
     */
    private final int start;

    /**
     * The formas of the voids of this body, in declaration order.
     */
    private final List<String> voids;

    /**
     * The protocol of the body.
     */
    private final Protocol steps;

    /**
     * Ctor.
     *
     * @param name The name of the helper, empty for the formation itself
     * @param offset The position of the first void among all voids
     * @param formas The formas of the voids, in declaration order
     * @param protocol The protocol of the body
     */
    Body(final String name, final int offset, final List<String> formas,
        final Protocol protocol) {
        this.label = name;
        this.start = offset;
        this.voids = formas;
        this.steps = protocol;
    }

    /**
     * The name of the helper this is the body of.
     *
     * @return The name, empty for the formation itself
     */
    String name() {
        return this.label;
    }

    /**
     * The position of the first void of this body among all voids.
     *
     * @return The offset
     */
    int offset() {
        return this.start;
    }

    /**
     * The formas of the voids of this body.
     *
     * @return The formas, in declaration order
     */
    List<String> formas() {
        return Collections.unmodifiableList(this.voids);
    }

    /**
     * The protocol of the body.
     *
     * @return The protocol
     */
    Protocol protocol() {
        return this.steps;
    }
}
