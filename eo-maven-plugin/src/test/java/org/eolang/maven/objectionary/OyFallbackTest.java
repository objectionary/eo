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
package org.eolang.maven.objectionary;

import java.io.IOException;
import java.util.concurrent.atomic.AtomicInteger;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Test for {@link OyFallback}.
 *
 * @since 1.0
 */
final class OyFallbackTest {

    /**
     * Primary objectionary.
     */
    private final OyMock primary = new OyMock("org.example.main");

    /**
     * Secondary objectionary.
     */
    private final OyMock secondary = new OyMock("org.example.secondary", "[] > s\n");

    /**
     * Objectionary with fallback.
     */
    private Objectionary fallback;

    @BeforeEach
    void setUp() {
        this.fallback = new OyFallback(this.primary, this.secondary);
    }

    @Test
    void getsObjectWhenPrimaryContains() throws Exception {
        MatcherAssert.assertThat(
            new TextOf(this.fallback.get("org.example.main")).asString(),
            Matchers.equalTo(this.primary.source)
        );
        MatcherAssert.assertThat(
            this.primary.invocations.get(),
            Matchers.equalTo(1)
        );
        MatcherAssert.assertThat(
            this.secondary.invocations.get(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void getsObjectWhenPrimaryNotContains() throws Exception {
        MatcherAssert.assertThat(
            new TextOf(this.fallback.get("org.example.secondary")).asString(),
            Matchers.equalTo(this.secondary.source)
        );
        MatcherAssert.assertThat(
            this.primary.invocations.get(),
            Matchers.equalTo(1)
        );
        MatcherAssert.assertThat(
            this.secondary.invocations.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void containsObject() throws IOException {
        MatcherAssert.assertThat(
            this.fallback.contains("org.example.main"),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            this.fallback.contains("org.example.absent"),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            this.primary.invocations.get(),
            Matchers.equalTo(2)
        );
        MatcherAssert.assertThat(
            this.secondary.invocations.get(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void convertsToString() {
        MatcherAssert.assertThat(
            this.fallback.toString(),
            Matchers.equalTo(String.format("[%s]+[fallback to %s]", this.primary, this.secondary))
        );
    }

    /**
     * Mock objectionary.
     *
     * @since 0.29.0
     */
    private static final class OyMock implements Objectionary {

        /**
         * Invocations counter.
         */
        private final AtomicInteger invocations;

        /**
         * Object name.
         */
        private final String name;

        /**
         * Object source.
         */
        private final String source;

        /**
         * Ctor.
         * @param object Object name
         */
        private OyMock(final String object) {
            this(object, "[] > main\n");
        }

        /**
         * Ctor.
         * @param object Object name
         * @param source Eo program source
         */
        private OyMock(final String object, final String source) {
            this.name = object;
            this.source = source;
            this.invocations = new AtomicInteger();
        }

        @Override
        public Input get(final String object) throws IOException {
            this.invocations.incrementAndGet();
            if (this.name.equals(object)) {
                return new InputOf(this.source);
            } else {
                throw new IOException(String.format("%s not found", object));
            }
        }

        @Override
        public boolean contains(final String object) {
            this.invocations.incrementAndGet();
            return this.name.equals(object);
        }
    }
}
