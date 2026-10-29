/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Values}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class ValuesTest {

    @Test
    void givesSameNumberToSameValue(@Mktmp final Path temp) throws IOException {
        final Values values = new Values(new Sources(temp).program());
        values.unknown("can't tell");
        MatcherAssert.assertThat(
            "the same value must get the same number again, but it didnt",
            values.unknown("can't tell"),
            Matchers.equalTo(values.unknown("can't tell"))
        );
    }

    @Test
    void namesCopyTheWayPythonEngineDid(@Mktmp final Path temp) throws IOException {
        final Values values = new Values(
            new Sources(temp, "pair.eo", "[left right] > pair\n  left > @\n").program()
        );
        MatcherAssert.assertThat(
            "a copy must be named formation@birthplace#tag{bound voids}, but it wasnt",
            values.full(
                values.intern(
                    new Value('O', "Φ.pair", "Φ.app.φ", 2L | Formation.RHO, "Φ.app", -1, 0)
                )
            ),
            Matchers.equalTo("Φ.pair@Φ.app.φ#Φ.app{right,ρ}")
        );
    }

    @Test
    void takesDecidedOpenVoidAsGap(@Mktmp final Path temp) throws IOException {
        final Values values = new Values(new Sources(temp).program());
        values.unknown("open void");
        MatcherAssert.assertThat(
            "an open void a stop decided must still be a gap, but it wasnt",
            values.gap(Values.JGAP),
            Matchers.is(true)
        );
    }

    @Test
    void opensVoidsCopyLeavesUnbound(@Mktmp final Path temp) throws IOException {
        final Values values = new Values(
            new Sources(temp, "pair.eo", "[left right] > pair\n  left > @\n").program()
        );
        MatcherAssert.assertThat(
            "the voids a copy leaves unbound must be open, but they werent",
            values.opened(new Value('O', "Φ.pair", "Φ.pair", 1L | Formation.RHO, "", -1, 0)),
            Matchers.contains("right")
        );
    }
}
