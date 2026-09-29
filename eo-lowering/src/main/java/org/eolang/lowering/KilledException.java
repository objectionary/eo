/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.io.IOException;

/**
 * A run of phino killed because it outlasted its budget of time.
 *
 * <p>It is an {@link IOException} so that it passes unchanged through a
 * footprint of the cache, which may throw nothing else, and so that a
 * killed run leaves nothing in the cache: the protocol is kept only after
 * the run that wrote it has come back.</p>
 *
 * @since 0.74.0
 */
final class KilledException extends IOException {

    /**
     * Serialization marker.
     */
    private static final long serialVersionUID = 0x4B696C6C6564L;

    /**
     * Ctor.
     *
     * @param message What was killed and after how long
     * @param cause The failure the kill was reported with
     */
    KilledException(final String message, final Throwable cause) {
        super(message, cause);
    }
}
