/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

import EOorg.EOeolang.EOerror;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

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
@Versionized
@SuppressWarnings("java:S5164")
public final class Dataized {

    /**
     * Dataization level.
     *
     * @todo #2251:90min It is necessary to call {@link ThreadLocal#remove()} on
     *  {@link Dataized#LEVEL} variables to prevent memory leaks. We should either find a place
     *  where this variable can be removed, or, if this is not possible
     *  (see https://github.com/objectionary/eo/pull/1930), come up with another solution.
     */
    private static final ThreadLocal<Integer> LEVEL = ThreadLocal.withInitial(() -> 0);

    /**
     * Max dataization level.
     *
     * @todo #2251:90min It is necessary to call {@link ThreadLocal#remove()} on
     *  {@link Dataized#MAX_LEVEL} variables to prevent memory leaks. We should either find a place
     *  where this variable can be removed, or, if this is not possible
     *  (see https://github.com/objectionary/eo/pull/1930), come up with another solution.
     */
    private static final ThreadLocal<Integer> MAX_LEVEL =
        ThreadLocal.withInitial(() -> Integer.getInteger("max.dataization.log", 3));

    /**
     * The object to datarize.
     */
    private final Phi phi;

    /**
     * Logger.
     */
    private final Logger logger;

    /**
     * Ctor.
     * @param src The object
     */
    public Dataized(final Phi src) {
        this(src, Logger.getLogger(Dataized.class.getName()));
    }

    /**
     * Ctor.
     * @param src The object
     * @param log Logger
     */
    public Dataized(final Phi src, final Logger log) {
        this.phi = src;
        this.logger = log;
    }

    /**
     * Extracts the data from the EO object as a byte array.
     *
     * <p>This method performs the dataization process, which involves converting
     * the EO object into a byte array. It logs the dataization process if the
     * logging level is set to FINE and the current dataization level is within
     * the maximum allowed level. If an error occurs during dataization, it logs
     * the error details and rethrows the exception.</p>
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
    @SuppressWarnings("PMD.PreserveStackTrace")
    public byte[] take() {
        final int before = Dataized.LEVEL.get();
        Dataized.LEVEL.set(before + 1);
        try {
            final byte[] data = this.phi.delta();
            if (this.logger.isLoggable(Level.FINE)
                && Dataized.LEVEL.get() <= Dataized.MAX_LEVEL.get()
            ) {
                this.logger.log(
                    Level.FINE,
                    String.format(
                        "%s\uD835\uDD3B( <%s>%s ) ➜ %s",
                        String.join("", Collections.nCopies(before, "·")),
                        this.phi.locator(),
                        this.phi.toString().replaceAll("[\n\t]", ""),
                        new BytesOf(data).asString().replaceAll("[\n\t]", "")
                    )
                );
            }
            return data;
        } catch (final EOerror.ExError ex) {
            final List<String> raw = new ArrayList<>(ex.messages().size());
            raw.addAll(ex.messages());
            Collections.reverse(raw);
            if ("org.eolang.string".equals(ex.enclosure().forma())) {
                raw.add(
                    String.format(
                        "\"%s\"",
                        new Dataized(ex.enclosure()).take(String.class)
                    )
                );
            }
            final String fmt = String.format("%%%dd) %%s", (int) Math.log10(raw.size()) + 1);
            final List<String> clean = new ArrayList<>(raw.size());
            int idx = 1;
            for (final String line : raw) {
                clean.add(String.format(fmt, idx, line));
                ++idx;
            }
            this.logger.log(
                Level.SEVERE,
                String.format(
                    "Dataized to org.eolang.error with %s inside, at:%n  ⇢ %s",
                    ex.enclosure().forma(),
                    String.join("\n  ⇢ ", clean)
                )
            );
            throw new EOerror.ExError(ex.enclosure());
        } finally {
            Dataized.LEVEL.set(before);
        }
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
            throw new IllegalArgumentException(
                String.format(
                    "Unknown type \"%s\", bytes are: %s",
                    type.getCanonicalName(),
                    Arrays.toString(this.asBytes().take())
                )
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
                "Can't dataize given bytes of length > 1 to boolean, bytes are: %s",
                Arrays.toString(weak)
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
