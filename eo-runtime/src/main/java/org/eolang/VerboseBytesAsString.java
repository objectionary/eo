/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.function.Supplier;

/**
 * Makes a {@link String} byte array that can represent bool,
 * double, int or {@link String}.
 *
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
     * @param data Data.
     */
    public VerboseBytesAsString(final byte[] data) {
        this.data = Arrays.copyOf(data, data.length);
    }

    @Override
    public String get() {
        final String result;
        switch (this.data.length) {
            case 0:
                result = "[<no bytes>]";
                break;
            case 1:
                result = String.format(
                    "[0x%02X] = %s",
                    this.data[0],
                    this.data[0] != 0
                );
                break;
            case 8:
                final Bytes bytes = new BytesOf(this.data);
                result = String.format(
                    "[0x%s] = %s, or \"%s\")",
                    this.toHex(),
                    bytes.asNumber(),
                    this.escaped()
                );
                break;
            default:
                result = String.format(
                    "[0x%s] = \"%s\"",
                    this.toHex(),
                    this.escaped()
                );
                break;
        }
        return result;
    }

    /**
     * Convert data to printable hex string.
     * @return String
     */
    private String toHex() {
        final char[] chars = new char[this.data.length * 2];
        for (int idx = 0; idx < this.data.length; ++idx) {
            final int value = this.data[idx] & 0xFF;
            chars[idx * 2] = VerboseBytesAsString.HEX_ARRAY[value >>> 4];
            chars[idx * 2 + 1] = VerboseBytesAsString.HEX_ARRAY[value & 0x0F];
        }
        return new String(chars).replaceAll("(.{8})", "$1-");
    }

    /**
     * Escape Java string.
     * @return Escaped one
     */
    private String escaped() {
        final char[] chars = new String(this.data, StandardCharsets.UTF_8).toCharArray();
        final StringBuilder out = new StringBuilder(chars.length);
        for (final char chr : chars) {
            if (chr < 0x20 || chr > 0x7f) {
                out.append(String.format("\\u%04x", (int) chr));
            } else {
                out.append(chr);
            }
        }
        return out.toString();
    }

}
