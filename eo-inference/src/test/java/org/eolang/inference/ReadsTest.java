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
 * Test case for {@link Reads}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class ReadsTest {

    @Test
    void readsTerminationOffDatum(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a read off a datum must be T, but it wasnt",
            new Settled(ReadsTest.world(temp)).state().attr(
                Values.DATA, "size", new Site("Φ.app.φ", "")
            ),
            Matchers.hasToString(new Ids(Values.TERM).toString())
        );
    }

    @Test
    void readsTerminationOffTermination(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a read off T must be T, but it wasnt",
            new Settled(ReadsTest.world(temp)).state().attr(
                Values.TERM, "size", new Site("Φ.app.φ", "")
            ),
            Matchers.hasToString(new Ids(Values.TERM).toString())
        );
    }

    @Test
    void readsUnboundVoidAsTermination(@Mktmp final Path temp) throws IOException {
        final World world = ReadsTest.world(temp);
        MatcherAssert.assertThat(
            "a void the copy leaves unbound must read T, but it didnt",
            new Settled(world).state().attr(
                world.intern(
                    new Value('O', "Φ.vase", "Φ.app.φ.ρ", 1L, "", -1, 0)
                ),
                "v🌵water", new Site("Φ.app.φ", "")
            ),
            Matchers.hasToString(new Ids(Values.TERM).toString())
        );
    }

    @Test
    void readsNameCompleteFormationLacksAsTermination(@Mktmp final Path temp)
        throws IOException {
        final World world = ReadsTest.world(temp);
        MatcherAssert.assertThat(
            "a read of a name a complete formation lacks must be T, but it wasnt",
            new Settled(world).state().attr(
                world.intern(new Value('O', "Φ.cat", "Φ.cat", 0L, "", -1, 0)),
                "bark", new Site("Φ.app.x", "")
            ),
            Matchers.hasToString(new Ids(Values.TERM).toString())
        );
    }

    @Test
    void readsCaretOfCopyWhoseFormationDeclaresNoneAsTermination(@Mktmp final Path temp)
        throws IOException {
        final World world = ReadsTest.world(temp);
        MatcherAssert.assertThat(
            "the ρ of a copy whose formation declares no ^ must read T, but it didnt",
            new Settled(world).state().attr(
                world.intern(new Value('O', "Φ.cat", "Φ.cat", 0L, "", -1, 0)),
                "ρ", new Site("Φ.app.x", "")
            ),
            Matchers.hasToString(new Ids(Values.TERM).toString())
        );
    }

    @Test
    void readsVoidTypeLeavesOutAsUnknownOnlyJavaFills(@Mktmp final Path temp)
        throws IOException {
        final State state = new Settled(ReadsTest.status(temp)).state();
        MatcherAssert.assertThat(
            "a void of a copy Java made that its type leaves out must read the unknown only Java fills, but it didnt",
            state.attr(
                state.java(new Annotation("Φ.os.status(Φ.number)")),
                "v🌵note", new Site("Φ.app.φ", "")
            ),
            Matchers.hasToString(new Ids(Values.JGAP).toString())
        );
    }

    @Test
    void readsVoidTypeGivesAsThatType(@Mktmp final Path temp) throws IOException {
        final State state = new Settled(ReadsTest.status(temp)).state();
        MatcherAssert.assertThat(
            "a void of a copy Java made must read what its type gives it, but it didnt",
            state.attr(
                state.java(new Annotation("Φ.os.status(Φ.number,Φ.string)")),
                "v🌵note", new Site("Φ.app.φ", "")
            ),
            Matchers.hasToString(new Ids(state.shared("Φ.string")).toString())
        );
    }

    private static World world(final Path temp) throws IOException {
        return new Sources(temp)
            .with("vase.eo", "[flower water] > vase", "  flower > @", "  water > wet")
            .with("app.eo", "[] > app", "  (vase 5).wet > @")
            .with("cat.eo", "[] > cat", "  \"meow\" > voice")
            .world();
    }

    private static World status(final Path temp) throws IOException {
        return new Sources(temp).with(
            "os.eo",
            "[] > os",
            "  [] > check /Q.os.status",
            "  [code note] > status",
            "    code > @",
            "    note > text"
        ).with(
            "report.eo",
            "[] > report",
            "  os.status 0 \"ok\" > @"
        ).world();
    }
}
