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

/**
 * Test case for {@link StEoLogged}.
 *
 * @since 0.30
 */
class StEoLoggedTest {

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
    void delegatesWithoutException() {
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
            "We expect that shift will throw an exception, but it didn't"
        );
        MatcherAssert.assertThat(
            String.format(
                "We expect that logs will contain the eo representation of the xml, but was %s",
                log.all()
            ),
            log.last(),
            Matchers.containsString(
                String.join(
                    "\n",
                    "[] > main",
                    "  TRUE > x",
                    "  FALSE > y"
                )
            )
        );
    }

    /**
     * Example xml.
     * <p>
     * {@code
     * [] > main
     *   TRUE > x
     *   FALSE > y
     * }
     * </p>
     *
     * @return XML
     */
    private static XML example() {
        return new XMLDocument(
            String.join(
                "\n",
                "<program>",
                "  <errors/>",
                "  <sheets/>",
                "  <objects>",
                "    <o abstract=\"\" line=\"1\" name=\"main\" pos=\"0\">",
                "      <o base=\"bool\" data=\"bytes\" line=\"2\" name=\"x\" pos=\"2\">01</o>",
                "      <o base=\"bool\" data=\"bytes\" line=\"3\" name=\"y\" pos=\"2\">00</o>",
                "    </o>",
                "  </objects>",
                "</program>"
            )
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
