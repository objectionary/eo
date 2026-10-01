/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;

/**
 * The error that says a run of phino was stopped because it took too long.
 *
 * <p>Every run of phino is given a limit of time. When a run is still
 * working after that time, phino stops by itself, writes a {@code timeout}
 * element at the end of its protocol, and fails. Then this error is
 * thrown.</p>
 *
 * <p>This error is an {@link IOException} for two reasons. First, the cache
 * that stores the results of phino lets only an {@link IOException} pass
 * through it. Second, the cache stores a result only when the run that
 * made it finishes normally. So, when a run is stopped, nothing is stored
 * in the cache, and the next build tries this run again.</p>
 *
 * @since 0.64.0
 */
final class KilledException extends IOException {

    /**
     * Serialization marker.
     */
    private static final long serialVersionUID = 0x4B696C6C6564L;

    /**
     * Ctor.
     *
     * @param message Which run was stopped, and after how much time
     * @param cause The error that reported that the run was stopped
     */
    KilledException(final String message, final Throwable cause) {
        super(message, cause);
    }
}
