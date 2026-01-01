/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.util.function.Supplier;

/**
 * Optional tojo hash.
 * Returns tojo hash if exists or empty string otherwise.
 * @since 0.41
 */
final class TojoHash implements Supplier<String> {
    /**
     * Tojo.
     */
    private final TjForeign tojo;

    /**
     * Ctor.
     * @param tjo Foreign tojo
     */
    TojoHash(final TjForeign tjo) {
        this.tojo = tjo;
    }

    @Override
    public String get() {
        final String hash;
        if (this.tojo.hasHash()) {
            hash = this.tojo.hash();
        } else {
            hash = "";
        }
        return hash;
    }
}
