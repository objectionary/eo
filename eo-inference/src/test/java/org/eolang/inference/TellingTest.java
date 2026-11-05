/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Telling}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class TellingTest {

    @Test
    void namesDecoratorByWhatItsAtGives(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a place that reads a decorator must be named by what its @ gives, but it wasnt",
            TellingTest.told(temp).sets().get("Φ.app.φ"),
            Matchers.equalTo(Map.of("Φ.number", 1))
        );
    }

    @Test
    void countsCopiesThatSayEachSet(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "each set must be counted by the copies that say it, but it wasnt",
            TellingTest.told(temp).sets().get("Φ.box.v🌵item"),
            Matchers.equalTo(Map.of("Φ.number", 1, "Φ.string", 1))
        );
    }

    @Test
    void tellsVoidNobodyFillsAsUnknown(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a void no call fills must be told as a void nobody fills, but it wasnt",
            TellingTest.told(temp).sets().get("Φ.box.v🌵spare"),
            Matchers.equalTo(Map.of("?unknown-empty", 2))
        );
    }

    @Test
    void tellsWhatCopiesOfFormationReduceTo(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "what the copies of a decorator reduce to must be told, but it wasnt",
            TellingTest.told(temp).reduction().get("Φ.five"),
            Matchers.equalTo(Map.of("Φ.number", 1))
        );
    }

    private static Told told(final Path temp) throws IOException {
        final World world = new Sources(temp).with(
            "five.eo",
            "[] > five",
            "  5 > @"
        ).with(
            "box.eo",
            "[item spare] > box",
            "  item > @",
            "  spare > other"
        ).with(
            "app.eo",
            "[] > app",
            "  five > @",
            "  (box 1).other > one",
            "  (box \"a\").other > word"
        ).world();
        final State state = new Settled(world).state();
        state.step();
        return new Telling(state, world).told();
    }

    @Test
    void tellsVoidOfJavaCopyByTheTypeItsAtomGives(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "a void of a copy Java made must be told by the type its atom gives, but it wasnt",
            TellingTest.status(temp).sets().get("Φ.os.status.v🌵code"),
            Matchers.equalTo(Map.of("Φ.number", 2))
        );
    }

    @Test
    void tellsVoidTypeLeavesOutAsUnknownOnlyJavaFills(@Mktmp final Path temp)
        throws IOException {
        MatcherAssert.assertThat(
            "a void of a copy Java made that its type leaves out must be told as the unknown only Java fills, but it wasnt",
            TellingTest.status(temp).sets().get("Φ.os.status.v🌵note"),
            Matchers.hasEntry("?unknown-java", 1)
        );
    }

    @Test
    void tellsEachOptionOfDeclaredUnion(@Mktmp final Path temp) throws IOException {
        final World world = new Sources(temp).with(
            "os.eo",
            "[] > os",
            "  [] > put /Q.number",
            "    ? > item /Q.bytes|Q.string"
        ).with(
            "app.eo",
            "[] > app",
            "  os.put 7 > @"
        ).world();
        final State state = new Settled(world).state();
        state.step();
        MatcherAssert.assertThat(
            "a void declared as a union must be told by each of its options, but it wasnt",
            new Telling(state, world).told().sets().get("Φ.os.put.v🌵item"),
            Matchers.hasKey("Φ.bytes\tΦ.number\tΦ.string")
        );
    }

    private static Told status(final Path temp) throws IOException {
        final World world = new Sources(temp).with(
            "os.eo",
            "[] > os",
            "  [] > check /Q.os.status(Q.number)",
            "  [code note] > status",
            "    code > @",
            "    note > text"
        ).with(
            "app.eo",
            "[] > app",
            "  os.check.text > @"
        ).with(
            "report.eo",
            "[] > report",
            "  os.status 0 \"ok\" > @"
        ).world();
        final State state = new Settled(world).state();
        state.step();
        return new Telling(state, world).told();
    }
}
