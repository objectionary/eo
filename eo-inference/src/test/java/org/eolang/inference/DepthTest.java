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
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Depth}.
 *
 * @since 0.69.0
 */
@ExtendWith(MktmpResolver.class)
final class DepthTest {

    @Test
    void understandsWhollyAProgramThatHidesNothing(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("cup.xmir"),
            String.join(
                "",
                "<object><o loc='Φ.cup' name='cup'>",
                "<o loc='Φ.cup.lid' name='lid'/></o></object>"
            )
        );
        new Resolved(new Clues()).follow(temp.resolve("xmirs"), temp.resolve("tables"));
        MatcherAssert.assertThat(
            "a program hiding nothing must come out at a hundred, but it didnt",
            new Depth(temp.resolve("xmirs"), temp.resolve("tables")).ladder().percent(),
            Matchers.closeTo(100.0d, 0.001d)
        );
    }

    @Test
    void leavesNothingUnknownInAProgramOfBytes(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("cup.xmir"),
            String.join(
                "",
                "<object><o loc='Φ.cup' name='cup'><o loc='Φ.cup.lid' name='lid'>",
                "<o loc='Φ.cup.lid.α0' as='α0'>01-</o></o></o></object>"
            )
        );
        new Resolved(new Clues()).follow(temp.resolve("xmirs"), temp.resolve("tables"));
        MatcherAssert.assertThat(
            "the bytes of a program must be understood as they stand, but they werent",
            new Depth(temp.resolve("xmirs"), temp.resolve("tables"))
                .ladder().rungs().get("nothing at all"),
            Matchers.equalTo(0)
        );
    }

    @Test
    void putsNamesTakenFromAVoidOnTheirOwnRung(@Mktmp final Path temp) throws IOException {
        Files.writeString(
            Files.createDirectories(temp.resolve("xmirs")).resolve("inc.xmir"),
            String.join(
                "",
                "<object><o loc='Φ.inc' name='inc'>",
                "<o base='∅' loc='Φ.inc.x' name='x'/>",
                "<o base='ξ.x' loc='Φ.inc.φ' name='φ'/></o></object>"
            )
        );
        new Resolved(new Clues()).follow(temp.resolve("xmirs"), temp.resolve("tables"));
        MatcherAssert.assertThat(
            "the void and what it answers for must stand apart, but they didnt",
            new Depth(temp.resolve("xmirs"), temp.resolve("tables"))
                .ladder().rungs().get("a name rooted at a void"),
            Matchers.equalTo(2)
        );
    }
}
