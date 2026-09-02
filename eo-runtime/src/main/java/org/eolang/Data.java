/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.nio.charset.StandardCharsets;

/**
 * A data container.
 * @since 0.1
 */
@FunctionalInterface
public interface Data {

    /**
     * Take the data.
     * @return The data
     */
    byte[] delta();

    /**
     * Makes a {@link Phi} out of a primitive Java object, like {@link String} or {@link Integer}.
     *
     * <p>This is more convenient than making EOstring, then making EObytes fill it up with data,
     * and then injecting bytes to string.
     * This class is used in Java tests mostly for the sake of brevity.
     * In auto-generated Java code we do:
     * {@code
     * Phi bytes = Phi.Φ.take("bytes").copy();
     * Phi attached = new PhApplication(bytes, 0, new byte[] {...});
     * Phi str = Phi.Φ.take("string").copy();
     * Phi applied = new PhApplication(str, 0, attached);
     * return applied;
     * }</p>
     *
     * <p>It never needs a receiver: data becomes one of the root objects — a
     * number, a string, a bytes, a tuple — and every one of them takes its
     * receiver from the package that handed it out.</p>
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
         *
         * <p>The data becomes an object right here, so that a type nobody
         * can convert is rejected at once, and not later, when the object
         * is dataized far away from the place that made it.</p>
         *
         * @param obj Data
         */
        public ToPhi(final Object obj) {
            this(Data.ToPhi.toPhi(obj));
        }

        /**
         * Ctor.
         * @param phi Already converted object
         */
        private ToPhi(final Phi phi) {
            this.object = phi;
        }

        @Override
        public Phi copy() {
            return this.object.copy();
        }

        @Override
        public boolean needsRho() {
            return false;
        }

        @Override
        public Phi take(final String name) {
            return this.object.take(name);
        }

        @Override
        public void put(final int pos, final Phi obj) {
            this.object.put(pos, obj);
        }

        @Override
        public void put(final String name, final Phi obj) {
            this.object.put(name, obj);
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
        public byte[] delta() {
            return this.object.delta();
        }

        @Override
        public Phi normalized() {
            return this.object.normalized();
        }

        @Override
        public String φTerm() {
            return this.object.φTerm();
        }

        private static Phi toPhi(final Object obj) {
            if (obj == null) {
                throw new IllegalArgumentException("Cannot convert null data to Phi");
            }
            final Phi phi;
            if (obj instanceof Boolean) {
                if (obj.equals(true)) {
                    phi = Phi.Φ.take("true");
                } else {
                    phi = Phi.Φ.take("false");
                }
            } else if (obj instanceof Phi[] elements) {
                Phi tuple = Phi.Φ.take("tuple").take("empty");
                int length = 0;
                for (final Phi element : elements) {
                    length += 1;
                    final Phi cell = Phi.Φ.take("tuple").copy();
                    cell.put(0, tuple);
                    cell.put(1, element);
                    cell.put(2, new Data.ToPhi(length));
                    tuple = cell;
                }
                phi = tuple;
            } else if (obj instanceof byte[] bytes) {
                phi = Phi.Φ.take("bytes").copy();
                phi.put(0, new PhDefault(bytes));
            } else if (obj instanceof Number number) {
                phi = Data.ToPhi.number(number.doubleValue());
            } else if (obj instanceof String text) {
                phi = Phi.Φ.take("string").copy();
                final Phi bts = Phi.Φ.take("bytes").copy();
                bts.put(0, new PhDefault(text.getBytes(StandardCharsets.UTF_8)));
                phi.put(0, bts);
            } else {
                throw new ExFailure(
                    "Unknown type of data: %s",
                    obj.getClass().getCanonicalName()
                );
            }
            return phi;
        }

        private static Phi number(final double value) {
            final Phi phi;
            if (Double.isNaN(value)) {
                phi = Phi.Φ.take("nan");
            } else if (value == Double.POSITIVE_INFINITY) {
                phi = Phi.Φ.take("pinf");
            } else if (value == Double.NEGATIVE_INFINITY) {
                phi = Phi.Φ.take("ninf");
            } else {
                phi = Phi.Φ.take("number").copy();
                final Phi bts = Phi.Φ.take("bytes").copy();
                bts.put(0, new PhDefault(new BytesOf(value).take()));
                phi.put(0, bts);
            }
            return phi;
        }
    }
}
