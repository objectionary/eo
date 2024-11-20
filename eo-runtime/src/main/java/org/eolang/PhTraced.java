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
 * Class to trace if the "cage.new" got into recursion during the dataization.
 *
 * <p>This class is thread safe in the meaning that different threads
 * can safely use different instances of the class and recursion
 * (If a thread dataizes the same recursively) will still be detected.</p>
 *
 * @since 0.36
 */
@Versionized
@SuppressWarnings("PMD.TooManyMethods")
public final class PhTraced implements Phi {

    /**
     * Name of property that responsible for keeping max depth.
     */
    public static final String RECURSION_LIMIT = "EO_MAX_CAGE_RECURSION_DEPTH";

    /**
     * Cages that are currently being dataized. If one cage is being datazed, and
     * it needs to be dataized inside current dataization, the locator of current object be here.
     *
     * @todo #2251:90min It is necessary to call {@link ThreadLocal#remove()} on
     *  {@link PhTraced#DATAIZING_CAGES} to prevent memory leaks. We should either find a
     *  place where this variable can be removed, or, if this is not possible
     *  (see https://github.com/objectionary/eo/pull/1930), come up with another solution.
     */
    @SuppressWarnings("java:S5164")
    private static final ThreadLocal<Map<Integer, Integer>> DATAIZING_CAGES = ThreadLocal
        .withInitial(HashMap::new);

    /**
     * Encaged object itself.
     */
    private final Phi object;

    /**
     * Locator of encaged object.
     */
    private final Integer locatr;

    /**
     * Max depth of cage recursion.
     */
    private final int depth;

    /**
     * Ctor.
     * @param object Encaged object
     * @param locator Locator of encaged object
     */
    public PhTraced(final Phi object, final Integer locator) {
        this(
            object,
            locator,
            Integer.parseInt(
                System.getProperty(PhTraced.RECURSION_LIMIT, "100")
            )
        );
    }

    /**
     * The main constructor.
     * @param object Encaged object
     * @param locator Locator of encaged object
     * @param depth Max depth of cage recursion
     */
    public PhTraced(final Phi object, final Integer locator, final int depth) {
        this.object = object;
        this.locatr = locator;
        this.depth = depth;
    }

    @Override
    public Phi copy() {
        return new PhTraced(this.object.copy(), this.locatr);
    }

    @Override
    public Phi take(final String name) {
        return new PhTraced.TracingWhileGetting<>(
            () -> this.object.take(name)
        ).get();
    }

    @Override
    public boolean put(final int pos, final Phi obj) {
        return new PhTraced.TracingWhileGetting<>(
            () -> this.object.put(pos, obj)
        ).get();
    }

    @Override
    public boolean put(final String name, final Phi obj) {
        return new PhTraced.TracingWhileGetting<>(
            () -> this.object.put(name, obj)
        ).get();
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
    public int hashCode() {
        return this.object.hashCode();
    }

    @Override
    public boolean equals(final Object obj) {
        return obj instanceof Phi && this.hashCode() == obj.hashCode();
    }

    @Override
    public byte[] delta() {
        return new TracingWhileGetting<>(this.object::delta).get();
    }

    /**
     * Supplier that traces the cage while gets.
     * NOT thread-safe.
     * @param <T> Type of return value
     * @since 0.36
     */
    private final class TracingWhileGetting<T> implements Supplier<T> {

        /**
         * Supplies the {@link Phi}.
         */
        private final Supplier<T> attr;

        /**
         * Ctor.
         * @param attr Supplier of the {@link Phi}.
         */
        private TracingWhileGetting(final Supplier<T> attr) {
            this.attr = attr;
        }

        @Override
        public T get() {
            final Integer incremented = this.incrementCageCounter();
            final T ret = this.attr.get();
            this.decrementCageCounter(incremented);
            return ret;
        }

        /**
         * Increments counter of cage in the {@link PhTraced#DATAIZING_CAGES}.
         * @return New value in the map.
         */
        private Integer incrementCageCounter() {
            return PhTraced.DATAIZING_CAGES.get().compute(
                PhTraced.this.locatr, (key, counter) -> {
                    final int ret = this.incremented(counter);
                    if (ret > PhTraced.this.depth) {
                        throw new ExFailure(
                            "The cage %s with locator %d has reached the maximum nesting depth = %d",
                            PhTraced.this.object,
                            PhTraced.this.locatr,
                            PhTraced.this.depth
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
         * Decrements counter in the {@link PhTraced#DATAIZING_CAGES}.
         * @param incremented Current value of counter. This argument ensures
         *  temporal coupling with {@link TracingWhileGetting#incrementCageCounter} method.
         */
        private void decrementCageCounter(final int incremented) {
            final int decremented = incremented - 1;
            if (decremented == 0) {
                PhTraced.DATAIZING_CAGES.get().remove(
                    PhTraced.this.locatr
                );
            } else {
                PhTraced.DATAIZING_CAGES.get().put(
                    PhTraced.this.locatr, decremented
                );
            }
        }
    }
}
