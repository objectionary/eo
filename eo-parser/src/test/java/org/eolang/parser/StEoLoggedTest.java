/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StFailure;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.Queue;
import java.util.function.Consumer;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.ImpossibleModificationException;
import org.xembly.Xembler;

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
            String.format(
                "We expect, that the uid() calculation will be delegated to the origin: %s",
                origin
            ),
            new StEoLogged(origin).uid(),
            Matchers.equalTo(origin.uid())
        );
    }

    @Test
    void delegatesWithoutException() throws ImpossibleModificationException {
        final FakeLog log = new FakeLog();
        MatcherAssert.assertThat(
            "We expect that shift will successfully generate output xml",
            new StEoLogged(new StUnhex(), log).apply(1, StEoLoggedTest.example()),
            Matchers.notNullValue()
        );
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
        final FakeLog log = new FakeLog();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new StEoLogged(new StFailure(), log).apply(1, StEoLoggedTest.example()),
            "We expect that shift will throw an exception, but xml didn't"
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

    /**
     * Example xml.
     * <p>
     * {@code
     * [] > main
     *   true > x
     *   false > y
     * }
     * </p>
     *
     * @return XML
     */
    private static XML example() throws ImpossibleModificationException {
        return new XMLDocument(
            new Xembler(
                new Directives().append(new DrProgram())
                    .add("o")
                    .attr("name", "bar")
            ).xml()
        );
    }

    /**
     * Fake log.
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
