/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Copies}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class CopiesTest {

    @Test
    void sharesOneNumberAmongCallsAtDifferentPlaces(@Mktmp final Path temp)
        throws IOException {
        final State state = new State(new Sources(temp).world());
        state.seed();
        MatcherAssert.assertThat(
            "two numbers made at two places must be the one shared copy, but they werent",
            state.copy(
                new Value("Φ.number", new Site("Φ.kiosk.φ", ""), 1L | Formation.RHO), Map.of()
            ),
            Matchers.equalTo(
                state.copy(
                    new Value("Φ.number", new Site("Φ.till.φ.α0", ""), 1L | Formation.RHO),
                    Map.of()
                )
            )
        );
    }

    @Test
    void givesBlankCopyToBareNameReadOffRoot(@Mktmp final Path temp) throws IOException {
        final World world = new Sources(temp).world();
        final State state = new State(world);
        state.seed();
        MatcherAssert.assertThat(
            "the bare name read off Φ must be the blank copy, but it wasnt",
            world.value(
                state.fresh("Φ.string", new Site("Φ.shop.φ.ρ", ""), Values.ROOT)
            ).birth(),
            Matchers.equalTo("blank")
        );
    }

    @Test
    void joinsBlankCopyToSharedOne(@Mktmp final Path temp) throws IOException {
        final State state = new State(new Sources(temp).world());
        state.seed();
        MatcherAssert.assertThat(
            "the blank copy must join the shared one, but it didnt",
            state.joined(
                state.fresh("Φ.bytes", new Site("Φ.box.φ", ""), Values.ROOT)
            ),
            Matchers.equalTo(state.shared("Φ.bytes"))
        );
    }

    @Test
    void keepsNoCanonWhenProgramLacksKindOfIt(@Mktmp final Path temp) throws IOException {
        final State state = new State(
            new World(
                new Sources(temp).program(),
                new Names(List.of("Φ.bytes", "Φ.money"), List.of(), List.of())
            )
        );
        state.seed();
        MatcherAssert.assertThat(
            "a program that lacks a kind of the canon must have no canon, but it had one",
            state.canon("Φ.bytes"),
            Matchers.is(false)
        );
    }

    @Test
    void namesCopyByWhatItIsMadeOf(@Mktmp final Path temp) throws IOException {
        final State state = new State(
            new Sources(temp).with("cup.eo", "[tea milk] > cup", "  tea > @").world()
        );
        state.seed();
        MatcherAssert.assertThat(
            "a copy made twice of the same things must be one copy, but it wasnt",
            state.copy(
                new Value("Φ.cup", new Site("Φ.tray.φ", "Φ.t"), 1L),
                Map.of("tea", new int[] {Values.DATA})
            ),
            Matchers.equalTo(
                state.copy(new Value("Φ.cup", new Site("Φ.tray.φ", "Φ.t"), 1L), Map.of())
            )
        );
    }

    @Test
    void dropsRhoHandedToCopyOfFormationThatDeclaresNone(@Mktmp final Path temp)
        throws IOException {
        final World world = new Sources(temp).with("lamp.eo", "[] > lamp", "  42 > @").world();
        final State state = new State(world);
        state.seed();
        MatcherAssert.assertThat(
            "a copy of a formation that declares no ^ must keep no ρ handed to it, but it kept one",
            world.full(state.fresh("Φ.lamp", new Site("Φ.desk.φ", ""), Values.ROOT)),
            Matchers.equalTo("Φ.lamp@Φ.desk.φ{}")
        );
    }

    @Test
    void keepsRhoHandedToCopyOfFormationThatDeclaresIt(@Mktmp final Path temp)
        throws IOException {
        final State state = new State(
            new Sources(temp).with("lamp.eo", "[^] > lamp", "  42 > @").world()
        );
        state.seed();
        MatcherAssert.assertThat(
            "a copy of a formation that declares ^ must keep the ρ handed to it, but it didnt",
            state.row(state.fresh("Φ.lamp", new Site("Φ.desk.φ", ""), Values.ROOT), "ρ").frozen(),
            Matchers.hasToString(new Ids(Values.ROOT).toString())
        );
    }

    @Test
    void bearsCopyJavaMakesWithBracketsOfItsType(@Mktmp final Path temp) throws IOException {
        final World world = CopiesTest.status(temp);
        final State state = new State(world);
        state.seed();
        MatcherAssert.assertThat(
            "the copy Java makes must be born with the brackets of its type, but it wasnt",
            world.full(state.java(new Annotation("Φ.os.status(Φ.number,Φ.string)"))),
            Matchers.equalTo("Φ.os.status@java(Φ.number,Φ.string){v🌵code,v🌵note}")
        );
    }

    @Test
    void keepsApartTwoTypesOfOneFormation(@Mktmp final Path temp) throws IOException {
        final State state = new State(CopiesTest.status(temp));
        state.seed();
        MatcherAssert.assertThat(
            "two types that put different things into one formation must make two copies, but they made one",
            state.java(new Annotation("Φ.os.status(Φ.string,Φ.bytes)")),
            Matchers.not(
                Matchers.equalTo(state.java(new Annotation("Φ.os.status(Φ.bytes,Φ.string)")))
            )
        );
    }

    @Test
    void fillsVoidOfTypeWithoutBracketsWithWhatCodePutsThere(@Mktmp final Path temp)
        throws IOException {
        final State state = new State(
            CopiesTest.called(temp, "[] > report", "  os.status 4 \"fine\" > @")
        );
        state.seed();
        MatcherAssert.assertThat(
            "a void of a type without brackets must hold what EO code puts into it, but it didnt",
            state.row(state.java(new Annotation("Φ.os.status")), "v🌵note").frozen(),
            Matchers.hasToString(new Ints(state.shared("Φ.string")).frozen().toString())
        );
    }

    @Test
    void leavesVoidNoCodeFillsUnbound(@Mktmp final Path temp) throws IOException {
        final World world = CopiesTest.status(temp);
        final State state = new State(world);
        state.seed();
        MatcherAssert.assertThat(
            "a void of a type without brackets that no code fills must stay unbound, but it was bound",
            world.opened(world.value(state.java(new Annotation("Φ.os.status")))),
            Matchers.contains("v🌵code", "v🌵note")
        );
    }

    @Test
    void putsJavaUnknownForFillingOnlyWalkAnswers(@Mktmp final Path temp)
        throws IOException {
        final State state = new State(
            CopiesTest.called(temp, "[x] > late", "  os.status 9 x > @")
        );
        state.seed();
        MatcherAssert.assertThat(
            "a filling only the walk answers must put the unknown only Java fills into its void, but it didnt",
            state.row(state.java(new Annotation("Φ.os.status")), "v🌵note").frozen(),
            Matchers.hasToString(new Ints(Values.JGAP).frozen().toString())
        );
    }

    @Test
    void keepsFixedFillingNextToJavaUnknownOfWalkOnlyOne(@Mktmp final Path temp)
        throws IOException {
        final State state = new State(
            CopiesTest.sources(temp)
                .with("report.eo", "[] > report", "  os.status 1 \"ok\" > @")
                .with("late.eo", "[x] > late", "  os.status 9 x > @")
                .world()
        );
        state.seed();
        MatcherAssert.assertThat(
            "a void with a filling the facts fix and one only the walk answers must hold both, but it didnt",
            state.row(state.java(new Annotation("Φ.os.status")), "v🌵note").frozen(),
            Matchers.hasToString(
                new Ints(state.shared("Φ.string"), Values.JGAP).frozen().toString()
            )
        );
    }

    @Test
    void givesFormationFillingCopyJavaMadeAsItsRho(@Mktmp final Path temp)
        throws IOException {
        final State state = new State(
            CopiesTest.called(
                temp, "[] > report", "  os.status > @", "    7", "    [^]", "      \"in\" > @"
            )
        );
        state.seed();
        final int copy = state.java(new Annotation("Φ.os.status"));
        MatcherAssert.assertThat(
            "a formation EO code puts into a void must take the copy Java made as its ρ, but it didnt",
            state.row(state.row(copy, "v🌵note").get(0), "ρ").frozen(),
            Matchers.hasToString(new Ints(copy).frozen().toString())
        );
    }

    @Test
    void leavesVoidOutOfBracketsUnbound(@Mktmp final Path temp) throws IOException {
        final World world = CopiesTest.status(temp);
        final State state = new State(world);
        state.seed();
        MatcherAssert.assertThat(
            "a void the brackets leave out must stay unbound, but it was bound",
            world.opened(world.value(state.java(new Annotation("Φ.os.status(Φ.bytes)")))),
            Matchers.contains("v🌵note")
        );
    }

    @Test
    void givesSharedCopyToKindOfCanonWhateverItsBrackets(@Mktmp final Path temp)
        throws IOException {
        final State state = new State(CopiesTest.status(temp));
        state.seed();
        MatcherAssert.assertThat(
            "Java must make the shared copy of a kind of the canon, but it made another",
            state.java(new Annotation("Φ.number(Φ.string)")),
            Matchers.equalTo(state.shared("Φ.number"))
        );
    }

    private static World called(final Path temp, final String... lines) throws IOException {
        return CopiesTest.sources(temp).with("report.eo", lines).world();
    }

    private static Sources sources(final Path temp) {
        return new Sources(temp).with(
            "os.eo",
            "[] > os",
            "  [] > check /Q.os.status",
            "  [code note] > status",
            "    code > @",
            "    note > text"
        );
    }

    private static World status(final Path temp) throws IOException {
        return CopiesTest.sources(temp).world();
    }
}
