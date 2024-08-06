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

/**
 * A data container.
 *
 * @since 0.1
 */
@Versionized
@SuppressWarnings("PMD.TooManyMethods")
public interface Data {
    /**
     * Attach data to the object.
     * @param data Data.
     * @todo #2931:60min Change the data storage architecture. Current implementation allows the
     *  presence of two methods for data manipulations: {@link Data#attach(byte[])} to set data and
     *  {@link Data#delta()} to get data; which does not seem to be object oriented. It also
     *  requires every object to have reserved place for possible injected data. In our case, every
     *  {@link PhDefault} has {@link PhDefault#data} variable. It would be much better to have this
     *  data only inside some decorator. The main difficulty here is - child attributes of
     *  decorated object should know that their \rho is decorated and contains data.
     */
    void attach(byte[] data);

    /**
     * Take the data.
     * @return The data
     */
    byte[] delta();

    /**
     * Makes a {@link Phi} out of a primitive Java object, like {@link String} or {@link Integer}.
     *
     * <p>This is more convenient than making EOstring, then making EObytes fill it up with data and
     * then injecting bytes to string.
     * This class is used in Java tests mostly for the sake of brevity.
     * In auto-generated Java code we use EOstring and then wrap it with {@link PhData}.
     *
     * @since 0.1
     */
    final class ToPhi implements Phi {
        /**
         * Phi object.
         */
        private final Phi object;

        /**
         * Ctor.
         * @param obj Data
         */
        public ToPhi(final Object obj) {
            this.object = Data.ToPhi.toPhi(obj);
        }

        @Override
        public boolean equals(final Object obj) {
            return this.object.equals(obj);
        }

        @Override
        public int hashCode() {
            return this.object.hashCode();
        }

        @Override
        public Phi copy() {
            return this.object.copy();
        }

        @Override
        public Phi take(final String name) {
            return this.object.take(name);
        }

        @Override
        public boolean put(final int pos, final Phi obj) {
            return this.object.put(pos, obj);
        }

        @Override
        public boolean put(final String name, final Phi obj) {
            return this.object.put(name, obj);
        }

        @Override
        public String locator() {
            return this.object.locator();
        }

        @Override
        public String forma() {
            return this.object.forma();
        }

        @Override
        public String φTerm() {
            return this.object.φTerm();
        }

        @Override
        public String toString() {
            return this.object.toString();
        }

        @Override
        public void attach(final byte[] data) {
            this.object.attach(data);
        }

        @Override
        public byte[] delta() {
            return this.object.delta();
        }

        /**
         * Convert to Phi object.
         * @param obj Object to convert
         * @return Constructed Phi
         */
        @SuppressWarnings("PMD.CognitiveComplexity")
        private static Phi toPhi(final Object obj) {
            final Phi phi;
            final Phi eolang = Phi.Φ.take("org").take("eolang");
            if (obj instanceof Boolean) {
                if (obj.equals(true)) {
                    phi = eolang.take("true");
                } else {
                    phi = eolang.take("false");
                }
            } else if (obj instanceof Phi[]) {
                final Phi tuple = eolang.take("tuple");
                Phi argument = tuple.take("empty");
                Phi tup;
                for (final Phi element : (Phi[]) obj) {
                    tup = tuple.copy();
                    tup.put(0, argument);
                    tup.put(1, element);
                    argument = tup;
                }
                phi = argument;
            } else if (obj instanceof byte[]) {
                phi = eolang.take("bytes").copy();
                phi.attach((byte[]) obj);
            } else {
                final byte[] bytes;
                if (obj instanceof Number) {
                    phi = eolang.take("number").copy();
                    bytes = new BytesOf(((Number) obj).doubleValue()).take();
                } else if (obj instanceof String) {
                    phi = eolang.take("string").copy();
                    bytes = ((String) obj).getBytes(StandardCharsets.UTF_8);
                } else {
                    throw new IllegalArgumentException(
                        String.format(
                            "Unknown type of data: %s",
                            obj.getClass().getCanonicalName()
                        )
                    );
                }
                final Phi bts = eolang.take("bytes").copy();
                bts.attach(bytes);
                phi.put(0, bts);
            }
            return phi;
        }
    }
}
