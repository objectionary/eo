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
package org.eolang;

import EOorg.EOeolang.EOnumber;
import EOorg.EOeolang.EOseq;
import java.util.HashMap;
import java.util.Map;
import org.cactoos.map.MapOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link UniverseDefault}.
 * @since 0.31
 */
final class UniverseDefaultTest {

    /**
     * Name of attribute bound with abstract object.
     */
    private static final String ABSTRACT_ATT = "abstract";

    /**
     * Name of dataizable attribute.
     */
    private static final String VALUE_ATT = "value";

    /**
     * Data byte array.
     */
    private static final byte[] DATA = new BytesOf(123_456_789L).take();

    @Test
    void findsSimpleAtt() {
        final Phi phi = new DummyWithAt(Phi.Φ);
        final UniverseDefault universe = new UniverseDefault(phi);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            universe.find(
                String.format("%s.%s.%s", "$", UniverseDefaultTest.ABSTRACT_ATT, Attr.RHO)
            ),
            Matchers.equalTo(
                phi.hashCode()
            )
        );
    }

    @Test
    void findsLongAtt() {
        final Phi phi = new DummyWithStructure();
        final UniverseDefault universe = new UniverseDefault(phi);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            universe.find(
                String.format(
                    "$.%s.%s.%s.%s",
                    UniverseDefaultTest.ABSTRACT_ATT,
                    UniverseDefaultTest.ABSTRACT_ATT,
                    Attr.RHO,
                    Attr.RHO
                    )
                ),
            Matchers.equalTo(
                phi.hashCode()
            )
        );
    }

    @Test
    void findsByAbsoluteLoc() {
        final Map<Integer, Phi> indexed = new HashMap<>();
        final UniverseDefault universe = new UniverseDefault(Phi.Φ, indexed);
        final int vertex = universe.find("Q.org.eolang.seq");
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            indexed.get(vertex).getClass(),
            Matchers.equalTo(EOseq.class)
        );
    }

    @Test
    void throwsIfWrongFind() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new UniverseDefault(
                new DummyWithStructure()
            ).find("$.wrong-name"),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void dataizesIndexed() {
        final UniverseDefault universe = new UniverseDefault(
            new DummyWithAt(Phi.Φ)
        );
        final int vertex = universe.find(
            "$.".concat(UniverseDefaultTest.VALUE_ATT)
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            universe.dataize(vertex),
            Matchers.equalTo(new BytesOf(1.0).take())
        );
    }

    @Test
    void throwsIfWrongDataize() {
        Assertions.assertThrows(
            ExAbstract.class,
            () -> new UniverseDefault(
                new DummyWithStructure()
            ).dataize(-1),
            AtCompositeTest.TO_ADD_MESSAGE
        );
    }

    @Test
    void copies() {
        final Universe universe = new UniverseDefault(
            new Data.ToPhi(123L)
        );
        final int origin = universe.find("$");
        final int copy = universe.copy(origin);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            copy,
            Matchers.not(origin)
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            universe.dataize(copy),
            Matchers.equalTo(
                universe.dataize(origin)
            )
        );
    }

    @Test
    void putsToCopy() {
        final Map<Integer, Phi> indexed = new HashMap<>();
        final Universe universe = new UniverseDefault(Phi.Φ, indexed);
        final int eobytes = universe.find("Q.org.eolang.bytes");
        final int copy = universe.copy(eobytes);
        universe.put(copy, UniverseDefaultTest.DATA);
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(indexed.get(copy)).take(),
            Matchers.equalTo(
                UniverseDefaultTest.DATA
            )
        );
    }

    @Test
    void bindsCopyToAbstract() {
        final Phi dummy = new DummyAbstract();
        final Map<Integer, Phi> indexed = new MapOf<>(dummy.hashCode(), dummy);
        final Universe universe = new UniverseDefault(dummy, indexed);
        final int eobytes = universe.find("Q.org.eolang.bytes");
        final int copy = universe.copy(eobytes);
        universe.put(copy, UniverseDefaultTest.DATA);
        universe.bind(
            dummy.hashCode(), copy, UniverseDefaultTest.ABSTRACT_ATT
        );
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new Dataized(dummy.take(UniverseDefaultTest.ABSTRACT_ATT)).take(),
            Matchers.equalTo(
                UniverseDefaultTest.DATA
            )
        );
    }

    /**
     * Dummy phi with plain attribute.
     * @since 0.31
     */
    private static class DummyWithAt extends PhDefault {

        /**
         * Ctor.
         * @param sigma Sigma.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        DummyWithAt(final Phi sigma) {
            this.add(
                UniverseDefaultTest.ABSTRACT_ATT,
                new AtComposite(sigma, self -> new EOnumber())
            );
            this.add(
                UniverseDefaultTest.VALUE_ATT,
                new AtComposite(sigma, self -> new Data.ToPhi(1L))
            );
        }
    }

    /**
     * Dummy phi with {@link DummyWithAt} as attribute.
     * @since 0.31
     */
    private static class DummyWithStructure extends PhDefault {

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        DummyWithStructure() {
            this.add(UniverseDefaultTest.ABSTRACT_ATT, new AtComposite(this, DummyWithAt::new));
        }
    }

    /**
     * Dummy phi with free attribute.
     * @since 0.31
     */
    private static class DummyAbstract extends PhDefault {

        /**
         * Ctor.
         */
        @SuppressWarnings("PMD.ConstructorOnlyInitializesOrCallOtherConstructors")
        DummyAbstract() {
            this.add(
                UniverseDefaultTest.ABSTRACT_ATT,
                new AtVoid(UniverseDefaultTest.ABSTRACT_ATT)
            );
        }
    }
}
