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

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collections;
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
 * <p>It's recommended to use {@link Param} object, when you are inside
 * a EO object: it will add type checking on top of dataization.
 *
 * @see <a href="https://arxiv.org/abs/2111.13384">Canonical explanation of the Dataization concept</a>
 * @since 0.1
 */
@Versionized
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
     * Extract the data from the object.
     * @return The data
     */
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
        } finally {
            Dataized.LEVEL.set(before);
        }
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
