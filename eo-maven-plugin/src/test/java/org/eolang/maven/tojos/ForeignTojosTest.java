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
package org.eolang.maven.tojos;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import org.cactoos.Func;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * Tests from {@link ForeignTojos}.
 *
 * @since 0.29.5
 */
final class ForeignTojosTest {

    /**
     * Testable foreign tojos.
     */
    private ForeignTojos tojos;

    /**
     * Set up environment before each test.
     */
    @BeforeEach
    void setUp() {
        this.tojos = new ForeignTojos();
    }

    @ParameterizedTest
    @CsvSource({
        "abs",
        "org.eolang.int",
        "QQ.io.stdout"
    })
    void contains(final String name) {
        this.tojos.add(name);
        MatcherAssert.assertThat(
            this.tojos.contains(name),
            Matchers.is(true)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "abs, sba",
        "org.eolang.int, org.eolang.float",
        "QQ.io.stdout, QQ.txt.sprintf"
    })
    void doesNotContain(final String existing, final String considered) {
        this.tojos.add(existing);
        MatcherAssert.assertThat(
            this.tojos.contains(considered),
            Matchers.is(false)
        );
    }

    @Test
    void findsLookingTojoCorrectly() {
        final String looking = "looking";
        MatcherAssert.assertThat(
            "Found tojo should be the same as added",
            this.tojos.add(looking),
            Matchers.equalTo(this.tojos.find(looking))
        );
    }

    @Test
    void throwsExceptionIfTojoWasNotFound() {
        final String id = "absent";
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> this.tojos.find(id),
            String.format("Should throw an exception if tojo with id='%s' was not found", id)
        );
    }

    @Test
    void findsAnyTojoIfSeveralTojosWithTheSameIdWereAdded() {
        final String same = "same";
        final ForeignTojo first = this.tojos.add(same);
        final ForeignTojo second = this.tojos.add(same);
        final List<ForeignTojo> expected = Arrays.asList(first, second);
        MatcherAssert.assertThat(
            "We don't care which tojo will be returned, but it should be one of the added tojos",
            expected,
            Matchers.hasItem(this.tojos.find(same))
        );
    }

    @ParameterizedTest
    @MethodSource("tojoFunctions")
    void throwsExceptionIfKeyWasNotFoundInTojo(
        final String key,
        final Func<ForeignTojo, Object> method) {
        final ForeignTojo tojo = this.tojos.add("string");
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> method.apply(tojo),
            String.format("Should throw an exception if key='%s' was not found in Tojo", key)
        );
        final IllegalStateException thrown = Assertions.assertThrows(
            IllegalStateException.class,
            tojo::verified
        );
        Assertions.assertEquals(
            "There is no 'VERIFIED' attribute in the tojo",
            thrown.getMessage(),
            "Should throw an exception if key 'VERIFIED' was not found in Tojo"
        );
    }

    @Test
    void findsKeyInTojo() {
        final ForeignTojo tojo = this.tojos.add("string");
        Assertions.assertEquals(
            tojo.identifier(),
            "string",
            "Shouldn't throw an exception if key 'ID' was found in Tojo"
        );
        Assertions.assertEquals(
            tojo.scope(),
            "compile",
            "Shouldn't throw an exception if key 'ID' was found in Tojo"
        );
        tojo.withVer("version");
        Assertions.assertEquals(
            tojo.ver(),
            "version",
            "Shouldn't throw an exception if key 'VER' was found in Tojo"
        );
    }

    @AfterEach
    void tearDown() throws IOException {
        this.tojos.close();
    }

    @SuppressWarnings("PMD.UnusedPrivateMethod")
    private static Stream<Arguments> tojoFunctions() {
        return Stream.of(
            Arguments.of("XMIR", (Func<ForeignTojo, Object>) ForeignTojo::xmir),
            Arguments.of("OPTIMIZED", (Func<ForeignTojo, Object>) ForeignTojo::optimized),
            Arguments.of("VERIFIED", (Func<ForeignTojo, Object>) ForeignTojo::verified),
            Arguments.of("SHAKEN", (Func<ForeignTojo, Object>) ForeignTojo::shaken),
            Arguments.of("EO", (Func<ForeignTojo, Object>) ForeignTojo::source),
            Arguments.of("VERSION", (Func<ForeignTojo, Object>) ForeignTojo::version),
            Arguments.of("ID", (Func<ForeignTojo, Object>) ForeignTojo::description),
            Arguments.of("HASH", (Func<ForeignTojo, Object>) ForeignTojo::hash),
            Arguments.of("PROBED", (Func<ForeignTojo, Object>) ForeignTojo::probed),
            Arguments.of("VER", (Func<ForeignTojo, Object>) ForeignTojo::ver)
        );
    }
}
