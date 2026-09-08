/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.util.Collections;
import java.util.List;

/**
 * One body of a program: the formation being lowered, or a recursive
 * helper of it, reduced over voids of its own.
 *
 * <p>The voids of every body are locals of one Java method, so they
 * stand in one list: the voids of the formation first, then those of
 * each helper body in the order the bodies were reached, and a body
 * knows the offset of its own. Its protocol is reduced once, over the
 * symbols of those voids, and each path of it either answers or resumes
 * a body, this one or another, with the values the voids of that body
 * take next.</p>
 *
 * @since 0.76.0
 */
public final class Body {

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
     * @param name The name of the helper, empty for the formation itself
     * @param offset The position of the first void among all voids
     * @param formas The formas of the voids, in declaration order
     * @param protocol The protocol of the body
     */
    public Body(final String name, final int offset, final List<String> formas,
        final Protocol protocol) {
        this.label = name;
        this.start = offset;
        this.voids = formas;
        this.steps = protocol;
    }

    /**
     * The name of the helper this is the body of.
     * @return The name, empty for the formation itself
     */
    public String name() {
        return this.label;
    }

    /**
     * The position of the first void of this body among all voids.
     * @return The offset
     */
    public int offset() {
        return this.start;
    }

    /**
     * The formas of the voids of this body.
     * @return The formas, in declaration order
     */
    public List<String> formas() {
        return Collections.unmodifiableList(this.voids);
    }

    /**
     * The protocol of the body.
     * @return The protocol
     */
    public Protocol protocol() {
        return this.steps;
    }
}
