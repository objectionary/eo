/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Random;

/**
 * TCP port from the unprivileged range.
 * @since 0.74.0
 */
public final class Port {

    /**
     * Port number.
     */
    private final int number;

    /**
     * Ctor, takes a port from the unprivileged range.
     */
    public Port() {
        this(new Random().nextInt(10_001) + 10_000);
    }

    /**
     * Ctor.
     * @param number Port number
     */
    private Port(final int number) {
        this.number = number;
    }

    /**
     * Port number.
     * @return Port number
     */
    public int number() {
        return this.number;
    }
}
