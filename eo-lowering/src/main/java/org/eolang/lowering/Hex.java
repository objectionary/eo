/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

/**
 * Bytes, as phino spells them: {@code 40-45-00-00-00-00-00-00}.
 *
 * <p>A single byte ends with a dash, {@code FF-}, and no bytes at all are
 * {@code --}, which is how a Δ formation with an empty payload prints.</p>
 *
 * @since 0.76.0
 */
public final class Hex {

    /**
     * The bytes.
     */
    private final byte[] data;

    /**
     * Ctor.
     *
     * @param dashed The text, such as {@code FF-} or {@code 40-45}
     */
    public Hex(final String dashed) {
        this(Hex.parsed(dashed));
    }

    /**
     * Ctor.
     *
     * @param value The number
     */
    public Hex(final double value) {
        this(ByteBuffer.allocate(Double.BYTES).putDouble(value).array());
    }

    /**
     * Ctor.
     *
     * @param value The truth
     */
    public Hex(final boolean value) {
        this(Hex.truth(value));
    }

    /**
     * Ctor.
     *
     * @param bytes The bytes
     */
    public Hex(final byte[] bytes) {
        this.data = bytes;
    }

    /**
     * The bytes.
     *
     * @return A copy of the bytes
     */
    public byte[] bytes() {
        return this.data.clone();
    }

    /**
     * The number these eight bytes spell.
     *
     * @return The number
     */
    public double number() {
        if (this.data.length != Double.BYTES) {
            throw new IllegalStateException(
                String.format("The bytes '%s' are not a number, since they are not eight", this.text())
            );
        }
        return ByteBuffer.wrap(this.data).getDouble();
    }

    /**
     * The text.
     *
     * @return The dashed text
     */
    public String text() {
        final String out;
        if (this.data.length == 0) {
            out = "--";
        } else if (this.data.length == 1) {
            out = String.format("%02X-", this.data[0] & 0xFF);
        } else {
            final List<String> pairs = new ArrayList<>(this.data.length);
            for (final byte octet : this.data) {
                pairs.add(String.format("%02X", octet & 0xFF));
            }
            out = String.join("-", pairs);
        }
        return out;
    }

    /**
     * Parse the text.
     *
     * @param dashed The text
     * @return The bytes
     */
    private static byte[] parsed(final String dashed) {
        final List<Byte> out = new ArrayList<>(0);
        for (final String pair : dashed.split("-", -1)) {
            if (!pair.isEmpty()) {
                out.add((byte) Integer.parseInt(pair, 16));
            }
        }
        final byte[] bytes = new byte[out.size()];
        for (int idx = 0; idx < bytes.length; ++idx) {
            bytes[idx] = out.get(idx);
        }
        return bytes;
    }

    /**
     * The byte of a truth.
     *
     * @param value The truth
     * @return One byte
     */
    private static byte[] truth(final boolean value) {
        final byte[] out;
        if (value) {
            out = new byte[] {(byte) 0xFF};
        } else {
            out = new byte[] {(byte) 0x00};
        }
        return out;
    }
}
