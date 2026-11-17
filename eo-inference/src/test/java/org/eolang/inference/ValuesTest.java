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
        final Values values = new Values(
            new Sources(temp).with("mug.eo", "[tea] > mug", "  tea > @").program()
        );
        values.intern(new Value('O', "Φ.mug", "Φ.app.φ", 1L, "", -1, 0));
        MatcherAssert.assertThat(
            "the same value must get the same number again, but it didnt",
            values.intern(new Value('O', "Φ.mug", "Φ.app.φ", 1L, "", -1, 0)),
            Matchers.equalTo(values.intern(new Value('O', "Φ.mug", "Φ.app.φ", 1L, "", -1, 0)))
        );
    }

    @Test
    void namesCopyTheWayPythonEngineDid(@Mktmp final Path temp) throws IOException {
        final Values values = new Values(
            new Sources(temp).with("pair.eo", "[left right] > pair", "  left > @").program()
        );
        MatcherAssert.assertThat(
            "a copy must be named formation@birthplace#tag{bound voids}, but it wasnt",
            values.full(
                values.intern(
                    new Value('O', "Φ.pair", "Φ.app.φ", 2L | Formation.RHO, "Φ.app", -1, 0)
                )
            ),
            Matchers.equalTo("Φ.pair@Φ.app.φ#Φ.app{v🌵right,ρ}")
        );
    }

    @Test
    void opensVoidsCopyLeavesUnbound(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the voids a copy leaves unbound must be open, but they werent",
            new Values(
                new Sources(temp).with("pair.eo", "[left right] > pair", "  left > @").program()
            ).opened(new Value('O', "Φ.pair", "Φ.pair", 1L | Formation.RHO, "", -1, 0)),
            Matchers.contains("v🌵right")
        );
    }
}
