/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.atoms;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.StringWriter;
import java.nio.file.Path;
import org.eolang.lowering.Symbols;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Dataizing}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class DataizingTest {

    @Test
    void answersBytesMarkerOfTarget(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(
            new Oracle(
                channel,
                "{\"𝑛\":\"Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S5 ⟧ ) )\",\"Φ.\":\"number\"}",
                "{\"λ\":\"S5\"}"
            )
        ).start();
        MatcherAssert.assertThat(
            "the dataized target must come back as bytes under the same symbol, but it didnt",
            new Dataizing(
                new Operands(1, channel, new Symbols(temp.resolve("s.tsv")))
            ).answer(),
            Matchers.equalTo("Φ.bytes( φ ↦ ⟦ λ ⤍ S5 ⟧ )")
        );
    }

    @Test
    void asksForTargetThatIsNotData(@Mktmp final Path temp) throws Exception {
        final Channel channel = new Channel(new StringWriter());
        new Thread(new Oracle(channel, "{\"𝑛\":\"ξ.ρ.x\"}", "{\"Δ\":\"2A-\"}")).start();
        MatcherAssert.assertThat(
            "a target that is not yet data must be reduced and answered as bytes, but it wasnt",
            new Dataizing(
                new Operands(1, channel, new Symbols(temp.resolve("s.tsv")))
            ).answer(),
            Matchers.equalTo("Φ.bytes( φ ↦ ⟦ Δ ⤍ 2A- ⟧ )")
        );
    }
}
