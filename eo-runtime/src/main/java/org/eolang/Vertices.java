/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.regex.Pattern;

/**
 * Collection of all vertices.
 *
 * The class is thread-safe.
 *
 * @since 0.18
 * @checkstyle
 */
final class Vertices {

    /**
     * Numbers of just objects.
     */
    private final AtomicInteger count = new AtomicInteger();

    /**
     * All seen.
     */
    private final ConcurrentHashMap<String, Integer> seen =
        new ConcurrentHashMap<>(0);

    /**
     * Get the next one.
     * @return Next vertex available
     */
    public int next() {
        return this.seen.computeIfAbsent(
            String.format("next:%d", this.count.addAndGet(1)),
            key -> this.seen.size() + 1
        );
    }

    /**
     * Get the best suitable one or next.
     * @param obj The object to find
     * @return Next vertex available or previously registered
     */
    public int best(final Object obj) {
        final String label = stringFrom(obj);
        final MessageDigest digest;
        try {
            digest = MessageDigest.getInstance("SHA-256");
        } catch (final NoSuchAlgorithmException ex) {
            throw new IllegalStateException(ex);
        }
        digest.update(String.format("%s %s", obj.getClass().getName(), label).getBytes());
        final String hash = new String(digest.digest());

        try {
            return Integer.class.cast(
                    new If(
                            obj instanceof Phi[],
                            this.next(),
                            this.seen.computeIfAbsent(
                                    hash, key -> this.seen.size() + 1
                            )
                    ).statement()
            );
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }


    private String stringFrom(Object obj) {
        try {
            return String.valueOf(
                    new If(
                            canToString(obj),
                            obj.toString(),
                            new If(
                                    obj instanceof Pattern,
                                    Pattern.class.cast(obj).pattern(),
                                    new If(
                                            obj instanceof byte[],
                                            Arrays.toString(byte[].class.cast(obj)),
                                            new IllegalArgumentException(
                                                    String.format(
                                                            "Unknown type for vertex allocation: %s",
                                                            obj.getClass().getCanonicalName()
                                                    )
                                            )
                                    ).statement()
                            ).statement()
                    ).statement()
            );
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }


    private boolean canToString(Object obj) {
        return obj instanceof Long || obj instanceof String || obj instanceof Character
                || obj instanceof Double || obj instanceof Boolean;
    }


    /**
     * This class is a replacement for the ternary operator
     * <p><p><b>Example:</b></p> expression ? obj1 : obj2; – ternary</p>
     * <p>new If(expression, obj1, obj2).statement(); - object analog</p>
     */
    private static final class If {
        final boolean expression;
        final Object phi;
        final Object sig;


        /**
         * Ctor
         * @param expression just expression with two conditions
         * @param phi statement of object if <b>expression</b> is <i>true</i>
         * @param sig statement of object if <b>expression</b> is <i>false</i>
         */
        public If(boolean expression, Object phi, Object sig) {
            this.expression = expression;
            this.phi = phi;
            this.sig = sig;
        }


        /**
         * @return statement of whole object
         * @throws Throwable that was passed instead as <b>phi</b> or <b>sig</b>
         */
        public Object statement() throws Throwable {
            tryThrow(phi);
            tryThrow(sig);

            return expression ? phi : sig;
        }


        private void tryThrow(Object obj) throws Throwable {
            if (obj instanceof Throwable) {
                throw Throwable.class.cast(obj);
            }
        }
    }
}
