package org.eolang.maven.footprint;

import java.io.File;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

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
        "'null', master, true",
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
        "0.0.0, abcdefg, 0.0.0/abcdefg",
        "2.0-SNAPSHOT, abcdefg, 2.0-SNAPSHOT/abcdefg",
        "1.0-SNAPSHOT, abcdefg, 1.0-SNAPSHOT/abcdefg",
        "SNAPSHOT, abcdefg, SNAPSHOT/abcdefg",
        "0.1.0, abcdefg, 0.1.0/abcdefg",
        "'', abcdefg, abcdefg",
        "'', master, master",
        "'','', ''",
        ",, ''",
        "'',, ''",
        ",,''",
    })
    void returnsCorrectCachePath(
        final String version,
        final String hash,
        final String expected
    ) {
        MatcherAssert.assertThat(
            new CacheVersion(version, hash).cache(),
            Matchers.equalTo(Paths.get(expected.replaceAll("/", File.separator)))
        );
    }
}