/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Arrays;

/**
 * Bytes, defensively copied on construction and on every read, so that
 * neither the source array nor a previously handed-out one can mutate
 * this object's view of its own data.
 * @since 0.1
 */
final class Snapshot {

    /**
     * The copied bytes, or {@code null} when none were given.
     */
    private final byte[] data;

    /**
     * Ctor.
     * @param data Bytes to copy, or {@code null}
     */
    Snapshot(final byte[] data) {
        if (data == null) {
            this.data = null;
        } else {
            this.data = Arrays.copyOf(data, data.length);
        }
    }

    /**
     * Whether no bytes were given.
     * @return True if empty
     */
    boolean empty() {
        return this.data == null;
    }

    /**
     * Whether bytes were given.
     * @return True if present
     */
    boolean present() {
        return this.data != null;
    }

    /**
     * A fresh copy of the bytes.
     * @return The copy, or {@code null} when {@link #empty()}
     */
    byte[] bytes() {
        final byte[] copy;
        if (this.data == null) {
            copy = null;
        } else {
            copy = Arrays.copyOf(this.data, this.data.length);
        }
        return copy;
    }
}
