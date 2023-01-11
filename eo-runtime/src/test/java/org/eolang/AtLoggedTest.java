/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtLogged}.
 *
 * @since 0.29
 */
class AtLoggedTest {

    /**
     * Testable object.
     */
    private AtLogged logged;

    /**
     * Delegate.
     */
    private AtSimple origin;

    /**
     * Output stream.
     */
    private ByteArrayOutputStream out;

    /**
     * Label for logging.
     */
    private String label;

    @BeforeEach
    void setUp() {
        this.out = new ByteArrayOutputStream();
        this.origin = new AtSimple();
        this.label = "test";
        this.logged = new AtLogged(this.origin, this.label, new PrintStream(this.out));
    }

    @Test
    void convertsToStringAsOrigin() {
        MatcherAssert.assertThat(
            this.logged.toString(),
            Matchers.equalTo(this.origin.toString())
        );
    }

    @Test
    void convertsToPhiTermAsOrigin() {
        MatcherAssert.assertThat(
            this.logged.φTerm(),
            Matchers.equalTo(this.origin.φTerm())
        );
    }

    @Test
    void copiesWithLogging() {
        this.logged.copy(Phi.Φ);
        final String log = new String(this.out.toByteArray(), StandardCharsets.UTF_8);
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.copy()...", this.label))
        );
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.copy()!", this.label))
        );
    }

    @Test
    void getsWithLogging() {
        MatcherAssert.assertThat(
            this.logged.get(),
            Matchers.equalTo(this.origin.get())
        );
        final String log = new String(this.out.toByteArray(), StandardCharsets.UTF_8);
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.get()...", this.label))
        );
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.get()!", this.label))
        );
    }

    @Test
    void putsWithLogging() {
        this.logged.put(Phi.Φ);
        final String log = new String(this.out.toByteArray(), StandardCharsets.UTF_8);
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.put()...", this.label))
        );
        MatcherAssert.assertThat(
            log,
            Matchers.containsString(String.format("  %s.put()!", this.label))
        );
    }
}
