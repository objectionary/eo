/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Random;

/**
 * A random port number, shared by socket syscall tests.
 * @since 0.40.0
 */
public final class RandomPort {

    /**
     * Random number generator.
     */
    private final Random random = new Random();

    /**
     * Pick a random port.
     * @return Random port
     */
    public int pick() {
        final int min = 10_000;
        return this.random.nextInt(20_000 - min + 1) + min;
    }
}
