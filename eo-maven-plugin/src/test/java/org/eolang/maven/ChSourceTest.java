/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test case for {@link ChSource}.
 *
 * @since 0.60
 */
@ExtendWith(MktmpResolver.class)
final class ChSourceTest {

    @Test
    void computesHashValueForTheFile(@Mktmp final Path dir) throws IOException {
        final Path file = dir.resolve("source.eo");
        Files.writeString(
            file,
            "[] > main\n  (stdout \"Hello, EO!\") > @\n"
        );
        MatcherAssert.assertThat(
            "We should compute the correct hash (SHA-1) for the source file",
            new ChSource(file).value(),
            Matchers.equalTo("f97a8da03d184cab9a43e1288293391fb1ccc296")
        );
    }

    @ParameterizedTest
    @ValueSource(
        strings = {
            "",
            " ",
            "    ",
            "\n",
            "\r\n",
            "\t",
            " \n\r\t "
        }
    )
    void computesHashForDifferentTrickyValues(final String original) {
        MatcherAssert.assertThat(
            "We should compute correct hash for tricky values",
            new ChSource(new TextOf(original)).value(),
            Matchers.allOf(
                Matchers.notNullValue(),
                Matchers.not(Matchers.blankString()),
                Matchers.hasLength(40)
            )
        );
    }

    @ParameterizedTest
    @CsvSource({
        "hello, aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d",
        "world, 7c211433f02071597741e6ff5a8ea34789abbf43",
        "eo-lang, 0af005396f4908b9a813149c18bb1220da6e1b3f",
        "Maven Plugin, 85c363ac6898a8462b017caaf972ac0e79ef8b2c"
    })
    void computesHashForDifferentValues(final String original, final String expected) {
        MatcherAssert.assertThat(
            "We should compute correct hash for different values",
            new ChSource(() -> original).value(),
            Matchers.equalTo(expected)
        );
    }
}
