/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Clues}.
 *
 * @since 0.67.0
 */
@ExtendWith(MktmpResolver.class)
final class CluesTest {

    @Test
    void followsEveryClue(@Mktmp final Path temp) throws IOException {
        new Clues(
            Arrays.asList(
                (xmirs, tables) -> Files.writeString(
                    Files.createDirectories(tables).resolve("first.xml"), "<first/>"
                ),
                (xmirs, tables) -> Files.writeString(
                    Files.createDirectories(tables).resolve("second.xml"), "<second/>"
                )
            )
        ).follow(temp.resolve("xmirs"), temp.resolve("tables"));
        MatcherAssert.assertThat(
            "every clue must be followed, but the last one was skipped",
            Files.exists(temp.resolve("tables").resolve("second.xml")),
            Matchers.is(true)
        );
    }

    @Test
    void writesTableOfWhatIsNeeded(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("cup.xmir"),
            String.join(
                "",
                "<object><o loc='Φ.cup.φ' base='.lid'>",
                "<o loc='Φ.cup.φ.ρ' base='ξ.cup'/></o></object>"
            )
        );
        new Clues().follow(temp.resolve("xmirs"), temp.resolve("tables"));
        MatcherAssert.assertThat(
            "the clues we know must write down what every object is asked for, but they didnt",
            Files.exists(temp.resolve("tables").resolve("needs.xml")),
            Matchers.is(true)
        );
    }

    @Test
    void writesTableOfWhatIsProvided(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("cup.xmir"),
            "<object><o loc='Φ.cup' name='cup'/></object>"
        );
        new Clues().follow(temp.resolve("xmirs"), temp.resolve("tables"));
        MatcherAssert.assertThat(
            "the clues we know must write down what every object provides, but they didnt",
            Files.exists(temp.resolve("tables").resolve("provides.xml")),
            Matchers.is(true)
        );
    }
}
