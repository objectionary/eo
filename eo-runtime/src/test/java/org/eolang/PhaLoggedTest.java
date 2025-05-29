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
 * Test case for {@link PhaLogged}.
 *
 * @since 0.29
 */
final class PhaLoggedTest {

    /**
     * Testable object.
     */
    private PhaLogged logged;

    /**
     * Object to test put.
     */
    private PhaLogged put;

    /**
     * Delegate.
     */
    private PhaSimple origin;

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
        this.origin = new PhaSimple();
        this.label = "test";
        final Logger mock = Logger.getLogger("mock");
        this.handler = new StreamHandler(this.out, new SimpleFormatter());
        mock.addHandler(this.handler);
        this.logged = new PhaLogged(this.origin, this.label, mock);
        this.put = new PhaLogged(new PhVoid("x"), this.label, mock);
    }

    @Test
    void copiesWithLogging() {
        this.logged.copy(Phi.Φ);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.copy()...", this.label))
        );
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.copy()!", this.label))
        );
    }

    @Test
    void getsWithLogging() {
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            this.logged.take(0),
            Matchers.equalTo(this.origin.take(0))
        );
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.get()...", this.label))
        );
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.get()!", this.label))
        );
    }

    @Test
    void putsWithLogging() {
        this.put.put("start", Phi.Φ);
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
            this.log(),
            Matchers.containsString(String.format("  %s.put()...", this.label))
        );
        MatcherAssert.assertThat(
            PhCompositeTest.TO_ADD_MESSAGE,
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
