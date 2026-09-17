/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.xml.XML;
import com.yegor256.xsline.StFailure;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Queue;
import java.util.function.Consumer;
import org.cactoos.Fallback;
import org.cactoos.io.InputOf;
import org.cactoos.scalar.ScalarWithFallback;
import org.cactoos.scalar.Unchecked;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link StEoLogged}.
 *
 * @since 0.30
 */
final class StEoLoggedTest {

    @Test
    void hasTheSameUid() {
        final StUnhex origin = new StUnhex();
        MatcherAssert.assertThat(
            "We expect that the uid() calculation will be delegated to the origin",
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

    @Test
    void printsMessageWithEoIfExceptionIsThrown() throws IOException {
        final StEoLoggedTest.FakeLog log = new StEoLoggedTest.FakeLog();
        final XML xml = StEoLoggedTest.example();
        new Unchecked<>(
            new ScalarWithFallback<>(
                () -> new StEoLogged(new StFailure(), log).apply(1, xml),
                new Fallback.From<>(Exception.class, ex -> xml)
            )
        ).value();
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

    private static XML example() throws IOException {
        return new EoSyntax(new InputOf(String.format("[] > bar%n"))).parsed();
    }

    /**
     * Fake log.
     *
     * <p>Used for testing purposes.</p>
     *
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
            this(new ArrayDeque<>(0));
        }

        /**
         * Ctor.
         *
         * @param captured Captured messages
         */
        private FakeLog(final Queue<String> captured) {
            this.captured = captured;
        }

        @Override
        public void accept(final String message) {
            this.captured.add(message);
        }

        private String last() {
            return this.captured.remove();
        }

        private boolean empty() {
            return this.captured.isEmpty();
        }

        private Collection<String> all() {
            return new ArrayList<>(this.captured);
        }
    }
}
