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
package org.eolang;

import EOorg.EOeolang.EOseq;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import java.util.HashMap;
import java.util.Map;

/**
 * Test case for {@link Universe}.
 * @since 0.31
 */
final class UniverseTest {

    /**
     * Name of attribute.
     */
    private static final String ATT = "value";

    @Test
    void findsSimpleAtt() {
        final Phi phi = new DummyWithAt(Phi.Φ);
        final Universe universe = new Universe(phi);
        MatcherAssert.assertThat(
            universe.find("$.".concat(UniverseTest.ATT)),
            Matchers.equalTo(
                phi.attr(UniverseTest.ATT).get().hashCode()
            )
        );
    }

    @Test
    void findsLongAtt() {
        final Phi phi = new DummyWithStructure(Phi.Φ);
        final Universe universe = new Universe(phi);
        MatcherAssert.assertThat(
            universe.find(
                String.format(
                    "$.%s.%s",
                    UniverseTest.ATT,
                    UniverseTest.ATT
                    )
                ),
            Matchers.equalTo(
                phi.attr(UniverseTest.ATT).get().attr(UniverseTest.ATT).get().hashCode()
            )
        );
    }

    @Test
    void findsByAbsoluteLoc() {
        final Map<Integer, Phi> indexed = new HashMap<>();
        final Universe universe = new Universe(Phi.Φ, indexed);
        final int vertex = universe.find("Q.org.eolang.seq");
        MatcherAssert.assertThat(
            indexed.get(vertex).getClass(),
            Matchers.equalTo(EOseq.class)
        );
    }

    @Test
    void throwsIfWrongFind() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Universe(
                new DummyWithStructure(Phi.Φ)
            ).find("$.wrong-name")
        );
    }

    @Test
    void dataizesIndexed() {
        final Universe universe = new Universe(
            new DummyWithAt(Phi.Φ)
        );
        final int vertex = universe.find(
            "$.".concat(UniverseTest.ATT)
        );
        MatcherAssert.assertThat(
            universe.dataize(vertex),
            Matchers.equalTo(new BytesOf(1L).take())
        );
    }

    @Test
    void throwsIfWrongDataize() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new Universe(
                new DummyWithStructure(Phi.Φ)
            ).dataize(-1)
        );
    }

    /**
     * Dummy phi with plain attribute.
     * @since 0.31
     */
    private static class DummyWithAt extends PhDefault {

        /**
         * Main ctor.
         * @param sigma Sigma.
         * @param att Att name.
         */
        DummyWithAt(final Phi sigma, final String att) {
            super(sigma);
            this.add(att, new AtComposite(sigma, self -> new Data.ToPhi(1L)));
        }

        /**
         * Ctor.
         * @param sigma Sigma.
         */
        DummyWithAt(final Phi sigma) {
            this(sigma, UniverseTest.ATT);
        }
    }

    /**
     * Dummy phi with {@link DummyWithAt} as attribute.
     * @since 0.31
     */
    private static class DummyWithStructure extends PhDefault {

        /**
         * Ctor.
         * @param sigma Sigma
         */
        DummyWithStructure(final Phi sigma) {
            super(sigma);
            this.add(UniverseTest.ATT, new AtComposite(this, DummyWithAt::new));
        }
    }
}
