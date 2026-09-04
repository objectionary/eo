/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.function.Supplier;

/**
 * Makes a {@link String} byte array that can represent bool,
 * double, int or {@link String}.
 * @since 0.36
 */
public final class VerboseBytesAsString implements Supplier<String> {

    /**
     * To help hex printing.
     */
    private static final char[] HEX_ARRAY = "0123456789ABCDEF".toCharArray();

    /**
     * Data.
     */
    private final byte[] data;

    /**
     * Ctor.
     * @param data Data
     */
    public VerboseBytesAsString(final byte[] data) {
        this.data = Arrays.copyOf(data, data.length);
    }

    @Override
    public String get() {
        final String result;
        if (this.data.length == 0) {
            result = "[<no bytes>]";
        } else if (this.data.length == 1) {
            result = String.format(
                "[0x%02X] = %s",
                this.data[0],
                this.data[0] == -1
            );
        } else if (this.data.length == Double.BYTES) {
            result = String.format(
                "[0x%s] = %s, or \"%s\"",
                this.toHex(),
                new BytesOf(this.data).asNumber(),
                this.escaped()
            );
        } else {
            result = String.format(
                "[0x%s] = \"%s\"",
                this.toHex(),
                this.escaped()
            );
        }
        return result;
    }

    private String toHex() {
        final char[] chars = new char[this.data.length * 2];
        for (int idx = 0; idx < this.data.length; ++idx) {
            final int value = this.data[idx] & 0xFF;
            chars[idx * 2] = VerboseBytesAsString.HEX_ARRAY[value >>> 4];
            chars[idx * 2 + 1] = VerboseBytesAsString.HEX_ARRAY[value & 0x0F];
        }
        return new String(chars).replaceAll("(.{8})(?=.)", "$1-");
    }

    private String escaped() {
        final char[] chars = new String(this.data, StandardCharsets.UTF_8).toCharArray();
        final StringBuilder out = new StringBuilder(chars.length);
        for (final char chr : chars) {
            if (chr < 0x20 || chr == 0x7F || chr > 0x7F) {
                out.append(String.format("\\u%04x", (int) chr));
            } else if (chr == '"' || chr == '\\') {
                out.append('\\').append(chr);
            } else {
                out.append(chr);
            }
        }
        return out.toString();
    }
}
