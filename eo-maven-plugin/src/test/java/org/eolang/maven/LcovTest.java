/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Lcov}.
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class LcovTest {

    @Test
    void namesEoSourceThatExistsOnDisk(@Mktmp final Path temp) throws Exception {
        final Path sources = temp.resolve("eo");
        Files.createDirectories(sources.resolve("числò"));
        final Path source = sources.resolve("числò/plus.eo");
        Files.write(source, "+package числò\n".getBytes(StandardCharsets.UTF_8));
        MatcherAssert.assertThat(
            "the .eo file the report names for the touched program does not exist on disk",
            new Lcov(sources, Collections.singleton("числò.plus:7:2")).toString(),
            Matchers.containsString(String.format("SF:%s%n", source))
        );
    }

    @Test
    void countsEveryTouchedPositionOfOneLine() {
        MatcherAssert.assertThat(
            "the two objects touched on one line are not counted into one line record",
            new Lcov(
                Paths.get("src/main/eo"),
                Arrays.asList("number:12:5", "number:12:9", "number:12:5")
            ).toString(),
            Matchers.equalTo(
                String.format(
                    "TN:%nSF:%s%nDA:12,2%nLF:1%nLH:1%nend_of_record%n",
                    Paths.get("src/main/eo").resolve("number.eo")
                )
            )
        );
    }

    @Test
    void printsNothingWhenNothingWasTouched() {
        MatcherAssert.assertThat(
            "a run that touched nothing is not reported as an empty tracefile",
            new Lcov(Paths.get("src/main/eo"), Collections.emptyList()).toString(),
            Matchers.equalTo("")
        );
    }

    @Test
    void skipsTornRecordAndKeepsTheRest() {
        MatcherAssert.assertThat(
            "the record torn by a concurrent append is not left out of the report",
            new Lcov(
                Paths.get("src/main/eo"),
                Arrays.asList("числò:4", "числò:9:1")
            ).toString(),
            Matchers.equalTo(
                String.format(
                    "TN:%nSF:%s%nDA:9,1%nLF:1%nLH:1%nend_of_record%n",
                    Paths.get("src/main/eo").resolve("числò.eo")
                )
            )
        );
    }
}
