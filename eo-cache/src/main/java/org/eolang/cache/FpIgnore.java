/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.cache;

/**
 * Footprint that does not update target path.
 *
 * @since 0.41
 */
public final class FpIgnore extends FpEnvelope {

    /**
     * Ctor.
     */
    public FpIgnore() {
        super((source, target) -> target);
    }
}
