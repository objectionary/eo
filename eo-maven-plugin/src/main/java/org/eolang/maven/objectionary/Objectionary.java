/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
package org.eolang.maven.objectionary;

import com.jcabi.log.Logger;
import java.io.IOException;
import org.cactoos.Func;
import org.cactoos.Input;
import org.cactoos.io.InputOf;

/**
 * Objectionary.
 *
 * @since 1.0
 */
public interface Objectionary {
    /**
     * Resolve object.
     *
     * @param name Object name.
     * @return Object code.
     * @throws IOException If fails to fetch.
     */
    Input get(String name) throws IOException;

    /**
     * Checks whether an Objectionary contains a provided object.
     * @param name Object name.
     * @return Object code.
     * @throws IOException If fails to fetch.
     */
    /**
     * Checks whether an Objectionary contains a provided object.
     *
     * @param name Object name.
     * @return Boolean: "true" if found, "false" if not.
     * @throws IOException If fails to fetch.
     */
    boolean contains(String name) throws IOException;

    /**
     * Objectionary with lambda-function Ctor-s for testing.
     *
     * @since 0.28.11
     * @checkstyle IllegalCatchCheck (150 lines)
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    final class Fake implements Objectionary {

        /**
         * Function that emulates 'get()' method in {@link Objectionary}.
         */
        private final Func<String, Input> getter;

        /**
         * Function that emulates 'contains()' method in {@link Objectionary}.
         */
        private final Func<String, Boolean> container;

        /**
         * Ctor.
         */
        public Fake() {
            this(
                s -> new InputOf("[] > sprintf\n")
            );
        }

        /**
         * Ctor.
         *
         * @param gett Lambda func for get()
         */
        Fake(final Func<String, Input> gett) {
            this(
                gett,
                s -> true
            );
        }

        /**
         * Ctor.
         *
         * @param gett Lambda func for get()
         * @param cont Lambda func for contains()
         */
        public Fake(
            final Func<String, Input> gett,
            final Func<String, Boolean> cont
        ) {
            this.getter = gett;
            this.container = cont;
        }

        @Override
        public Input get(final String name) {
            try {
                return this.getter.apply(name);
            } catch (final Exception ex) {
                Logger.debug(
                    this, "Invalid lambda function for get() method in OyFake!"
                );
                throw new IllegalArgumentException(ex);
            }
        }

        @Override
        public boolean contains(final String name) {
            try {
                return this.container.apply(name);
            } catch (final Exception ex) {
                Logger.debug(
                    this, "Invalid lambda function for contains() method in OyFake!"
                );
                throw new IllegalArgumentException(ex);
            }
        }

        @Override
        public String toString() {
            return "OyFake";
        }
    }
}
