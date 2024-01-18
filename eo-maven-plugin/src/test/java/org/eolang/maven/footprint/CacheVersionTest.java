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
package org.eolang.maven.footprint;

import java.io.File;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test cases for {@link CacheVersion}.
 *
 * @since 0.30
 */
class CacheVersionTest {

    @ParameterizedTest
    @CsvSource({
        "0.0.0, abcdefg, false",
        "2.0-SNAPSHOT, abcdefg, false",
        "1.0-SNAPSHOT, abcdefg, false",
        "SNAPSHOT, abcdefg, false",
        "'','', false",
        "0.1.0, '', false",
        "0.1.0, abcdefg, true",
        "'', abcdefg, true",
        "'', master, true",
        "'null', master, true"
    })
    void checksIfVersionIsCacheable(
        final String version,
        final String hash,
        final boolean expected
    ) {
        MatcherAssert.assertThat(
            new CacheVersion(version, hash).cacheable(),
            Matchers.is(expected)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "0.0.0, abcdefg, 0.0.0|abcdefg",
        "2.0-SNAPSHOT, abcdefg, 2.0-SNAPSHOT|abcdefg",
        "1.0-SNAPSHOT, abcdefg, 1.0-SNAPSHOT|abcdefg",
        "SNAPSHOT, abcdefg, SNAPSHOT|abcdefg",
        "0.1.0, abcdefg, 0.1.0|abcdefg",
        "'', abcdefg, abcdefg",
        "'', master, master",
        "'','', ''",
        ",, ''",
        "'',, ''",
        ",,''"
    })
    void returnsCorrectCachePath(
        final String version,
        final String hash,
        final String expected
    ) {
        MatcherAssert.assertThat(
            new CacheVersion(version, hash).path(),
            Matchers.equalTo(Paths.get(expected.replace("|", File.separator)))
        );
    }
}
