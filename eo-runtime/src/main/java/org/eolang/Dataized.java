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
     */
    private static final ThreadLocal<Integer> LEVEL = ThreadLocal.withInitial(() -> 0);

    /**
     * Max dataization level.
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
     * Take the object, no matter the type.
     * @return The data
     */
    public byte[] take() {
        final int before = Dataized.LEVEL.get();
        Dataized.LEVEL.set(before + 1);
        try {
            Phi src = this.phi;
            if (!(src instanceof Data)) {
                src = src.attr("Δ").get();
                if (!(src instanceof Data)) {
                    throw new IllegalStateException(
                        String.format(
                            "The attribute Δ of %s has %s instead of %s",
                            this.phi.getClass().getCanonicalName(),
                            src.getClass().getCanonicalName(),
                            Data.class.getCanonicalName()
                        )
                    );
                }
            }
            final Object data = Data.class.cast(src).take();
            if (!(data instanceof byte[])) {
                throw new ExFailure(
                    "data of %s must be %s, but was %s",
                    this.phi.toString(),
                    byte[].class,
                    data.getClass()
                );
            }
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
                        new Data.Value<>(data).toString().replaceAll("[\n\t]", "")
                    )
                );
            }
            return (byte[]) data;
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
        final byte[] weak = this.take();
        final Object strong;
        if (type.equals(Long.class)) {
            strong = new BytesOf(weak).asNumber(Long.class);
        } else if (type.equals(Double.class)) {
            strong = new BytesOf(weak).asNumber(Double.class);
        } else if (type.equals(byte[].class)) {
            strong = weak;
        } else if (type.equals(String.class)) {
            strong = new String(weak, StandardCharsets.UTF_8);
        } else if (weak.length == 1 && type.equals(Boolean.class)) {
            if (weak[0] == 1) {
                strong = true;
            } else {
                strong = false;
            }
        } else {
            throw new IllegalArgumentException(
                String.format("Unknown type: %s", type.getCanonicalName())
            );
        }
        return type.cast(strong);
    }

    /**
     * Clean up resources.
     * This includes call of {@link ThreadLocal#remove()} method to prevent
     * memory leaks.
     */
    public static void cleanUp() {
        Dataized.LEVEL.remove();
        Dataized.MAX_LEVEL.remove();
    }

}
