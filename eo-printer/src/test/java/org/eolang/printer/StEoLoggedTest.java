/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.xsline.StFailure;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Queue;
import java.util.function.Consumer;
import org.cactoos.Proc;
import org.cactoos.io.InputOf;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link StEoLogged}.
 * @since 0.30
 */
final class StEoLoggedTest {

    @Test
    void hasTheSameUid() {
        final StUnhex origin = new StUnhex();
        MatcherAssert.assertThat(
            String.format(
                "We expect, that the uid() calculation will be delegated to the origin: %s",
                origin
            ),
            new StEoLogged(origin).uid(),
            Matchers.equalTo(origin.uid())
        );
    }

    @Test
    void delegatesWithoutException() throws IOException {
        MatcherAssert.assertThat(
            "We expect that shift will successfully generate output xml",
            new StEoLogged(new StUnhex(), new StEoLoggedTest.FakeLog())
                .apply(1, StEoLoggedTest.example()),
            Matchers.notNullValue()
        );
    }

    @Test
    void delegatesWithoutLogs() throws IOException {
        final StEoLoggedTest.FakeLog log = new StEoLoggedTest.FakeLog();
        new StEoLogged(new StUnhex(), log).apply(1, StEoLoggedTest.example());
        MatcherAssert.assertThat(
            String.format(
                "We expect that logs will be empty, but was %s",
                log.all()
            ),
            log.empty(),
            Matchers.is(true)
        );
    }

    /**
     * Check EO log message on exception thrown.
     */
    @Test
    void printsMessageWithEoIfExceptionIsThrown() {
        final StEoLoggedTest.FakeLog log = new StEoLoggedTest.FakeLog();
        StEoLoggedTest.safe(
            ignore -> new StEoLogged(new StFailure(), log)
                .apply(1, StEoLoggedTest.example())
        );
        MatcherAssert.assertThat(
            String.format(
                "We expect that logs will contain the eo representation of the xml, but was %s",
                log.all()
            ),
            log.last(),
            Matchers.containsString("[] > bar")
        );
    }

    @Test
    void throwsExceptionIfFailure() {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new StEoLogged(new StFailure(), new StEoLoggedTest.FakeLog())
                .apply(1, StEoLoggedTest.example()),
            "We expect that shift will throw an exception, but xml didn't"
        );
    }

    /**
     * Example XMIR.
     * <p>
     * {@code
     * [] > bar
     * }
     * </p>
     * @return XML
     * @throws IOException If fails to parse
     */
    private static XML example() throws IOException {
        return new EoSyntax(new InputOf(String.format("[] > bar%n"))).parsed();
    }

    /**
     * Ignores all excesptions thrown.
     * @param run Procedure to execute
     * @checkstyle IllegalCatchCheck (10 lines)
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    private static void safe(final Proc<Object> run) {
        try {
            run.exec(new Object());
        } catch (final Exception ignore) {
            Logger.trace(StEoLoggedTest.class, ignore.getMessage());
        }
    }

    /**
     * Fake log.
     * <p>Used for testing purposes.</p>
     * @since 0.30
     */
    private static final class FakeLog implements Consumer<String> {

        /**
         * Captured messages.
         */
        private final Queue<String> captured;

        /**
         * Ctor.
         */
        private FakeLog() {
            this(new LinkedList<>());
        }

        /**
         * Ctor.
         * @param captured Captured messages
         */
        private FakeLog(final Queue<String> captured) {
            this.captured = captured;
        }

        @Override
        public void accept(final String message) {
            this.captured.add(message);
        }

        /**
         * Get last captured message.
         * @return Captured message
         */
        private String last() {
            return this.captured.remove();
        }

        /**
         * Check if captured messages are empty.
         * @return True if empty, false otherwise
         */
        private boolean empty() {
            return this.captured.isEmpty();
        }

        /**
         * Get all captured messages.
         * @return Captured messages
         */
        private Collection<String> all() {
            return new ArrayList<>(this.captured);
        }
    }
}
