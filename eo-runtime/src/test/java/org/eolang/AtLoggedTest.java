/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.logging.StreamHandler;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link AtLogged}.
 *
 * @since 0.29
 */
final class AtLoggedTest {

    /**
     * Testable object.
     */
    private AtLogged logged;

    /**
     * Object to test put.
     */
    private AtLogged put;

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

    /**
     * Logger handler.
     * Allows writing logs to output stream.
     */
    private StreamHandler handler;

    @BeforeEach
    void setUp() {
        this.out = new ByteArrayOutputStream();
        this.origin = new AtSimple();
        this.label = "test";
        final Logger mock = Logger.getLogger("mock");
        this.handler = new StreamHandler(this.out, new SimpleFormatter());
        mock.addHandler(this.handler);
        this.logged = new AtLogged(this.origin, this.label, mock);
        this.put = new AtLogged(new AtVoid("x"), this.label, mock);
    }

    @Test
    void copiesWithLogging() {
        this.logged.copy(Phi.Φ);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.copy()...", this.label))
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.copy()!", this.label))
        );
    }

    @Test
    void getsWithLogging() {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            this.logged.get(),
            Matchers.equalTo(this.origin.get())
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.get()...", this.label))
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.get()!", this.label))
        );
    }

    @Test
    void putsWithLogging() {
        this.put.put(Phi.Φ);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.put()...", this.label))
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.put()!", this.label))
        );
    }

    /**
     * We need `flush` logs to output stream.
     * Because <a href="https://stackoverflow.com/questions/69978526/why-streamhandler-doesnt-catch-log-messages">it's default behaviour.</a>
     * @return Last log message.
     */
    private String log() {
        this.handler.flush();
        return new String(this.out.toByteArray(), StandardCharsets.UTF_8);
    }
}
