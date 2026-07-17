/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

/**
 * Which optional diagnostic artifacts the transpiler emits: transformation
 * steps (intermediate XMIR dumps) and source locations (PhSafe wrappers).
 * @since 0.73.1
 */
final class Tracking {

    /**
     * Whether to track transformation steps into intermediate XMIR files.
     */
    private final boolean steps;

    /**
     * Whether to wrap dispatched objects with location info.
     */
    private final boolean locations;

    /**
     * Ctor.
     * @param steps Whether to track transformation steps
     * @param locations Whether to wrap dispatched objects with location info
     */
    Tracking(final boolean steps, final boolean locations) {
        this.steps = steps;
        this.locations = locations;
    }

    /**
     * Whether to track transformation steps into intermediate XMIR files.
     * @return True if steps are tracked
     */
    boolean steps() {
        return this.steps;
    }

    /**
     * Whether to wrap dispatched objects with location info.
     * @return True if locations are tracked
     */
    boolean locations() {
        return this.locations;
    }
}
