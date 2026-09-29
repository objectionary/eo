/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import org.cactoos.list.ListOf;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Pruning}.
 *
 * @since 0.74.0
 */
@ExtendWith(MktmpResolver.class)
final class PruningTest {

    @Test
    void cutsACheckOutOfAnObject(@Mktmp final Path temp) throws IOException {
        final Path home = temp.resolve("lower");
        new Pruning(
            Collections.singletonList(
                PruningTest.parsed(
                    temp.resolve("flag.xmir"),
                    String.format(
                        "[a] > flag%n  a.not > @%n  ++> can-flip-a-lie%n    true > @%n"
                    )
                )
            ),
            home
        ).exec();
        MatcherAssert.assertThat(
            "the copy must hold no test, but it does",
            new XMLDocument(home.resolve("flag.xmir"))
                .nodes("//o[starts-with(@name, 'p🌵')]"),
            Matchers.empty()
        );
    }

    @Test
    void cutsAFailingCheckOutOfAnObject(@Mktmp final Path temp) throws IOException {
        final Path home = temp.resolve("lower");
        new Pruning(
            Collections.singletonList(
                PruningTest.parsed(
                    temp.resolve("gate.xmir"),
                    String.format(
                        "[a] > gate%n  a.not > @%n  --> stops-on-a-missing-void%n    gate.plus 7 > @%n"
                    )
                )
            ),
            home
        ).exec();
        MatcherAssert.assertThat(
            "the copy must hold no failing test, but it does",
            new XMLDocument(home.resolve("gate.xmir"))
                .nodes("//o[starts-with(@name, 'n🌵')]"),
            Matchers.empty()
        );
    }

    @Test
    void cutsACheckOutOfANestedFormation(@Mktmp final Path temp) throws IOException {
        final Path home = temp.resolve("lower");
        new Pruning(
            Collections.singletonList(
                PruningTest.parsed(
                    temp.resolve("outer.xmir"),
                    String.format(
                        String.join(
                            "%n",
                            "[a] > outer",
                            "  inner > @",
                            "  [b] > inner",
                            "    b.times 3 > @",
                            "    ++> can-triple-eleven",
                            "      33.eq (inner 11) > @",
                            ""
                        )
                    )
                )
            ),
            home
        ).exec();
        MatcherAssert.assertThat(
            "the copy must hold no test at any depth, but it does",
            new XMLDocument(home.resolve("outer.xmir"))
                .nodes("//o[starts-with(@name, 'p🌵')]"),
            Matchers.empty()
        );
    }

    @Test
    void keepsTheBindingsAroundTheCheck(@Mktmp final Path temp) throws IOException {
        final Path home = temp.resolve("lower");
        new Pruning(
            Collections.singletonList(
                PruningTest.parsed(
                    temp.resolve("pair.xmir"),
                    String.format(
                        String.join(
                            "%n",
                            "[x y] > pair",
                            "  x.plus y > @",
                            "  ++> can-add-two-and-five",
                            "    7.eq (pair 2 5) > @",
                            "  x.minus y > gap",
                            ""
                        )
                    )
                )
            ),
            home
        ).exec();
        MatcherAssert.assertThat(
            "the bindings on both sides of the test must stay, but they dont",
            new XMLDocument(home.resolve("pair.xmir"))
                .nodes("/object/o[@name='pair'][o[@name='φ']][o[@name='gap']][count(o) = 4]"),
            Matchers.not(Matchers.empty())
        );
    }

    @Test
    void namesEveryCopyAfterItsSourceInOrder(@Mktmp final Path temp) throws IOException {
        final Path home = temp.resolve("lower");
        MatcherAssert.assertThat(
            "the copies must be named after the sources and sorted, but they arent",
            new Pruning(
                new ListOf<>(temp.resolve("zeta.xmir"), temp.resolve("alpha.xmir")),
                home
            ).paths(),
            Matchers.contains(
                home.resolve("alpha.xmir"), home.resolve("zeta.xmir")
            )
        );
    }

    @Test
    void failsNamingTheSourceThatTakesTheNameOfAnother(@Mktmp final Path temp)
        throws IOException {
        final Path first = PruningTest.parsed(
            Files.createDirectories(temp.resolve("one")).resolve("twin.xmir"),
            String.format("[a] > twin%n  a > @%n")
        );
        final Path second = PruningTest.parsed(
            Files.createDirectories(temp.resolve("two")).resolve("twin.xmir"),
            String.format("[b] > twin%n  b > @%n")
        );
        MatcherAssert.assertThat(
            "the failure must name the file two sources are named after, but it doesnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                () -> new Pruning(new ListOf<>(first, second), temp.resolve("lower")).exec(),
                "two sources of one name must fail the pruning"
            ).getMessage(),
            Matchers.containsString("twin.xmir")
        );
    }

    private static Path parsed(final Path file, final String program) throws IOException {
        return Files.write(
            file, new EoSyntax(program).parsed().toString().getBytes(StandardCharsets.UTF_8)
        );
    }
}
