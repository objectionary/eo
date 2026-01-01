/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Arrays;
import java.util.StringJoiner;
import java.util.function.Supplier;

/**
 * Bytes to hex converter.
 *
 * @since 0.44
 */
final class BytesToHex implements Supplier<String> {

    /**
     * The bytes.
     */
    private final byte[] bytes;

    /**
     * Ctor.
     * @param bts Bytes
     */
    BytesToHex(final byte[] bts) {
        this.bytes = Arrays.copyOf(bts, bts.length);
    }

    @Override
    public String get() {
        final String hex;
        if (this.bytes.length == 0) {
            hex = "--";
        } else {
            final StringJoiner out = new StringJoiner("-");
            for (final byte bty : this.bytes) {
                out.add(String.format("%02X", bty));
            }
            if (this.bytes.length == 1) {
                out.add("");
            }
            hex = out.toString();
        }
        return hex;
    }
}
