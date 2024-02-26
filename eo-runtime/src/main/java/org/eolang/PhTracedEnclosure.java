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

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

/**
 * Class to trace if the cage got into recursion during the dataization.
 * NOT thread-safe.
 * @since 0.36
 * @todo #2836:60min Make the class thread safe. It has private static
 *  field which can be accessed from differ thread and is not thread safe.
 *  Needs to synchronize this field.
 */
@Versionized
public final class PhTracedEnclosure implements Phi {

    /**
     * Name of property that responsible for keeping max depth.
     */
    public static final String MAX_CAGE_RECURSION_DEPTH_PROPERTY_NAME = "EO_MAX_CAGE_RECURSION_DEPTH";

    /**
     * Cages that are currently dataizing. If one cage is datazing, and
     * it needs to be dataized inside current dataizing, the cage will be here.
     */
    private static final Map<Phi, Integer> DATAIZING_CAGES = new HashMap<>();

    /**
     * Enclosure.
     */
    private final Phi enclosure;

    /**
     * Vertex of cage where the {@link PhTracedEnclosure#enclosure}
     * was retrieved.
     */
    private final Phi cage;

    /**
     * Max depth of cage recursion.
     */
    private final int depth;

    /**
     * Ctor.
     * @param enclosure Enclosure.
     * @param cage Vertex of source cage.
     */
    public PhTracedEnclosure(final Phi enclosure, final Phi cage) {
        this(
            enclosure,
            cage,
            Integer.parseInt(
                System.getProperty(PhTracedEnclosure.MAX_CAGE_RECURSION_DEPTH_PROPERTY_NAME, "100")
            )
        );
    }

    /**
     * The main constructor.
     * @param enclosure Enclosure.
     * @param cage Cage.
     * @param depth Max depth of cage recursion.
     */
    public PhTracedEnclosure(final Phi enclosure, final Phi cage, final int depth) {
        this.enclosure = enclosure;
        this.cage = cage;
        this.depth = depth;
    }

    @Override
    public Phi copy() {
        return new PhTracedEnclosure(this.enclosure.copy(), this.cage);
    }

    @Override
    public String locator() {
        return this.enclosure.locator();
    }

    @Override
    public String forma() {
        return this.enclosure.forma();
    }

    @Override
    public String φTerm() {
        return this.enclosure.φTerm();
    }

    @Override
    public Attr attr(final int pos) {
        return new PhTracedEnclosure.TracingWhileGetting(
            () -> this.enclosure.attr(pos)
        ).get();
    }

    @Override
    public Attr attr(final String name) {
        return new PhTracedEnclosure.TracingWhileGetting(
            () -> this.enclosure.attr(name)
        ).get();
    }

    @Override
    public int hashCode() {
        return this.enclosure.hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
        return obj instanceof Phi && this.hashCode() == obj.hashCode();
    }

    /**
     * Supplier that traces the cage while gets.
     * NOT thread-safe.
     * @since 0.36
     */
    private final class TracingWhileGetting implements Supplier<Attr> {

        /**
         * Supplies the {@link Attr}.
         */
        private final Supplier<Attr> attr;

        /**
         * Ctor.
         * @param attr Supplier of the {@link Attr}.
         */
        private TracingWhileGetting(final Supplier<Attr> attr) {
            this.attr = attr;
        }

        @Override
        public Attr get() {
            final Integer incremented = this.incrementCageCounter();
            final Attr ret = this.attr.get();
            this.decrementCageCounter(incremented);
            return ret;
        }

        /**
         * Increments counter of cage in the {@link PhTracedEnclosure#DATAIZING_CAGES}.
         * @return New value in the map.
         */
        private Integer incrementCageCounter() {
            return PhTracedEnclosure.DATAIZING_CAGES.compute(
                PhTracedEnclosure.this.cage, (key, counter) -> {
                    final int ret = this.incremented(counter);
                    if (ret > PhTracedEnclosure.this.depth) {
                        throw new ExFailure(
                            "The cage %s has reached the maximum nesting depth = %d",
                            key.φTerm(),
                            PhTracedEnclosure.this.depth
                        );
                    }
                    return ret;
                }
            );
        }

        /**
         * Creates incremented number.
         * @param number Number.
         * @return Incremented number. 1 if number is null.
         * @checkstyle NonStaticMethodCheck (10 lines). Static declarations in
         *  inner classes are not supported at language level '8'.
         */
        private Integer incremented(final Integer number) {
            final int ret;
            if (number == null) {
                ret = 1;
            } else {
                ret = number + 1;
            }
            return ret;
        }

        /**
         * Decrements counter in the {@link PhTracedEnclosure#DATAIZING_CAGES}.
         * @param incremented Current value of counter. This argument ensures
         *  temporal coupling with {@link TracingWhileGetting#incrementCageCounter} method.
         */
        private void decrementCageCounter(final int incremented) {
            final int decremented = incremented - 1;
            if (decremented == 0) {
                PhTracedEnclosure.DATAIZING_CAGES.remove(
                    PhTracedEnclosure.this.cage
                );
            } else {
                PhTracedEnclosure.DATAIZING_CAGES.put(
                    PhTracedEnclosure.this.cage, decremented
                );
            }
        }
    }
}
