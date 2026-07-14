/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

/**
 * A dataized object.
 *
 * <p>The class automates the process of turning EO objects into data. The
 * mechanism is explained in details in our canonical paper. Simply put,
 * it makes an attempt to either type-cast the provided object into data
 * or find "Δ" attribute inside it. If neither of that works, there is a
 * runtime exception.
 *
 * @see <a href="https://arxiv.org/abs/2111.13384">Canonical explanation of the Dataization concept</a>
 * @since 0.1
 */
@SuppressWarnings("java:S5164")
public final class Dataized {

    /**
     * The object to dataize.
     */
    private final Phi phi;

    /**
     * Ctor.
     * @param src The object
     */
    public Dataized(final Phi src) {
        this.phi = src;
    }

    /**
     * Extracts the data from the EO object as a byte array.
     *
     * <p>This method performs the dataization process, which involves converting
     * the EO object into a byte array. If the object cannot be dataized — for
     * example when it is a terminated computation (⊥) — the failure propagates
     * as an {@link ExFailure} and is not caught here.</p>
     *
     * <p>Usage example:</p>
     *
     * <pre>{@code
     * Phi phi = ...; // Initialize your EO object
     * Dataized dataized = new Dataized(phi);
     * byte[] data = dataized.take();
     * }</pre>
     *
     * @return The data
     */
    public byte[] take() {
        return this.phi.delta();
    }

    /**
     * Take the data with a type.
     * @param type The type
     * @param <T> The type
     * @return The data
     */
    public <T> T take(final Class<T> type) {
        final Object res;
        if (type.equals(Long.class)) {
            res = new BytesOf(this.take()).asNumber(Long.class);
        } else if (type.equals(Double.class)) {
            res = this.asNumber();
        } else if (type.equals(Integer.class)) {
            res = new BytesOf(this.take()).asNumber(Integer.class);
        } else if (type.equals(Short.class)) {
            res = new BytesOf(this.take()).asNumber(Short.class);
        } else if (type.equals(byte[].class)) {
            res = this.take();
        } else if (type.equals(String.class)) {
            res = this.asString();
        } else if (type.equals(Boolean.class)) {
            res = this.asBool();
        } else {
            throw new ExFailure(
                "Unknown type \"%s\", bytes are: %s",
                type.getCanonicalName(),
                Arrays.toString(this.asBytes().take())
            );
        }
        return type.cast(res);
    }

    /**
     * Extract the data from the object and convert to string.
     * @return Data as string
     */
    public String asString() {
        return new String(this.take(), StandardCharsets.UTF_8);
    }

    /**
     * Extract the data from the object and convert to number.
     * @return Data as number
     */
    public Double asNumber() {
        return new BytesOf(this.take()).asNumber();
    }

    /**
     * Extract the data from the object and convert to boolean.
     * @return Data as boolean
     */
    public Boolean asBool() {
        final byte[] weak = this.take();
        if (weak.length != 1) {
            throw new ExFailure(
                "Can't dataize bytes of length %d to boolean, exactly 1 byte is required, bytes are: %s",
                weak.length, Arrays.toString(weak)
            );
        }
        return weak[0] == 1;
    }

    /**
     * Extract the data from the object and convert to {@link Bytes}.
     * @return Data as {@link Bytes}
     */
    public Bytes asBytes() {
        return new BytesOf(this.take());
    }
}
