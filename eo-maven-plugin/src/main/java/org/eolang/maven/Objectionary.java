/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.io.IOException;
import org.cactoos.Func;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.cactoos.scalar.Unchecked;

/**
 * Objectionary.
 *
 * @since 0.1.0
 */
interface Objectionary {
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
        private final Func<? super String, ? extends Input> getter;

        /**
         * Function that emulates 'contains()' method in {@link Objectionary}.
         */
        private final Func<? super String, Boolean> container;

        /**
         * Ctor.
         */
        Fake() {
            this(
                s -> new InputOf(
                    String.join(
                        "\n",
                        "# No comments.",
                            "[] > sprintf\n"
                    )
                )
            );
        }

        /**
         * Ctor.
         *
         * @param gett Lambda func for get()
         */
        Fake(final Func<? super String, ? extends Input> gett) {
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
        Fake(
            final Func<? super String, ? extends Input> gett,
            final Func<? super String, Boolean> cont
        ) {
            this.getter = gett;
            this.container = cont;
        }

        @Override
        public Input get(final String name) {
            return new Unchecked<>(() -> this.getter.apply(name)).value();
        }

        @Override
        public boolean contains(final String name) {
            return new Unchecked<>(() -> this.container.apply(name)).value();
        }

        @Override
        public String toString() {
            return "OyFake";
        }
    }
}
