/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * The state of a walk over a world, with the canon made and the passes
 * run, the way {@link Walk} leaves it before the end step.
 *
 * @since 0.76.0
 */
final class Settled {

    /**
     * The world the walk goes over.
     */
    private final World world;

    /**
     * Ctor.
     *
     * @param over The world the walk goes over
     */
    Settled(final World over) {
        this.world = over;
    }

    /**
     * The state of the walk, settled.
     *
     * @return The state
     */
    State state() {
        final State state = new State(this.world);
        state.seed();
        state.settle();
        return state;
    }
}
