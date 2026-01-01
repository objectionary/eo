/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

/**
 * Footprint that does not update target path.
 * @since 0.41
 */
final class FpIgnore extends FpEnvelope {
    /**
     * Ctor.
     */
    FpIgnore() {
        super((source, target) -> target);
    }
}
