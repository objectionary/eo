/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

/**
 * The end step (design doc 4.6): what is decided once after the passes,
 * with nothing fed back.
 *
 * <p>It tells which study copies no longer count (3.4): a study copy counts
 * only if its formation never got a real copy and the copy it was made
 * inside counts.</p>
 *
 * @since 0.76.0
 */
final class EndStep {

    /**
     * The state of the walk.
     */
    private final State state;

    /**
     * Ctor.
     *
     * @param walk The state of the walk
     */
    EndStep(final State walk) {
        this.state = walk;
    }

    /**
     * Tell which study copies no longer count.
     */
    void step() {
        this.state.conclude();
    }
}
