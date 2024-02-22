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

import java.util.HashSet;
import java.util.Set;
import java.util.function.Supplier;

/**
 * Class to trace if the cage got into recursion during the dataization.
 * @since 0.36
 * @todo #2836:60min Add a new parameter of recursion depth. This parameter
 *   should be set by user via pom.xml. We can make DATAIZING_CAGES a
 *   Map and count how many  times the cage was met.
 * @todo #2836:60min Make the class thread safe. It has private static
 *  field which can be accessed from differ thread and is not thread safe.
 *  Needs to synchronize this field.
 */
@Versionized
public final class PhTracedEnclosure implements Phi {

    /**
     * Cages that are currently dataizing. If one cage is datazing and
     * it needs to be dataized inside current dataizing, the cage will be here.
     */
    private static final Set<Integer> DATAIZING_CAGES = new HashSet<>();

    /**
     * Enclosure.
     */
    private final Phi enclosure;

    /**
     * Vertex of cage where the {@link PhTracedEnclosure#enclosure}
     * was retrieved.
     */
    private final int cage;

    /**
     * Ctor.
     * @param enclosure Enclosure.
     * @param cage Vertex of source cage.
     */
    public PhTracedEnclosure(final Phi enclosure, final int cage) {
        this.enclosure = enclosure;
        this.cage = cage;
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
            if (PhTracedEnclosure.DATAIZING_CAGES.contains(PhTracedEnclosure.this.cage)) {
                throw new ExFailure(
                    "The cage %s is already dataizing",
                    PhTracedEnclosure.this.cage
                );
            }
            PhTracedEnclosure.DATAIZING_CAGES.add(PhTracedEnclosure.this.cage);
            final Attr ret = this.attr.get();
            PhTracedEnclosure.DATAIZING_CAGES.remove(PhTracedEnclosure.this.cage);
            return ret;
        }
    }
}
