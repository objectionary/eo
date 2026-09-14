/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.w3c.dom.Document;

/**
 * Test case for {@link Applied}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class AppliedTest {

    @Test
    void seedsVoidsOfFragmentAndOfItsParent(@Mktmp final Path temp) throws IOException {
        final Symbols symbols = new Symbols(temp.resolve("s.tsv"));
        final Map<String, String> given = new HashMap<>(2);
        given.put("Φ.foo.f.x", "number");
        given.put("Φ.foo.n", "bool");
        new Applied(
            AppliedTest.doc(), "Φ.foo.f", new Formas(Collections.emptyMap(), given), symbols
        ).phi();
        MatcherAssert.assertThat(
            "the voids must be seeded with their paths from the fragment, but they werent",
            symbols.rows(),
            Matchers.contains(
                Arrays.asList("S1", "number", "void", "x"),
                Arrays.asList("S2", "bool", "void", "ρ.n")
            )
        );
    }

    @Test
    void appliesEveryFormationOnTheWayToItsMarkers(@Mktmp final Path temp) throws IOException {
        final Map<String, String> given = new HashMap<>(2);
        given.put("Φ.foo.f.x", "number");
        given.put("Φ.foo.n", "bool");
        MatcherAssert.assertThat(
            "the way in must apply each formation to the markers of its voids, but it doesnt",
            new Applied(
                AppliedTest.doc(), "Φ.foo.f", new Formas(Collections.emptyMap(), given),
                new Symbols(temp.resolve("s.tsv"))
            ).phi(),
            Matchers.equalTo(
                String.join(
                    "",
                    "Φ.foo( n ↦ Φ.bool( if ↦ ⟦ left ↦ ∅, right ↦ ∅, guard ↦ ⟦ λ ⤍ S2 ⟧, ",
                    "λ ⤍ L_fork ⟧ ) ).f( x ↦ Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S1 ⟧ ) ) )"
                )
            )
        );
    }

    @Test
    void leavesReceiverVoidToTheDispatch(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the ρ void must stay for the dispatch to bind, but it was applied",
            new Applied(
                AppliedTest.doc(), "Φ.foo.f",
                new Formas(Collections.emptyMap(), Collections.emptyMap()),
                new Symbols(temp.resolve("s.tsv"))
            ).phi(),
            Matchers.not(Matchers.containsString("ρ ↦"))
        );
    }

    @Test
    void refusesAbsentFragment(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Applied(
                AppliedTest.doc(), "Φ.foo.z",
                new Formas(Collections.emptyMap(), Collections.emptyMap()),
                new Symbols(temp.resolve("s.tsv"))
            ).phi(),
            "a locator the document lacks cannot be entered, but it was"
        );
    }

    @Test
    void seedsReceiverOfDataMethodAsItsCarrier(@Mktmp final Path temp) throws IOException {
        final Symbols symbols = new Symbols(temp.resolve("s.tsv"));
        new Applied(
            AppliedTest.data(), "Φ.number.minus",
            new Formas(Collections.emptyMap(), Collections.emptyMap()), symbols
        ).phi();
        MatcherAssert.assertThat(
            "the receiver of a method of a data forma must be seeded as a value of it, but it wasnt",
            symbols.rows(),
            Matchers.hasItem(Arrays.asList("S2", "number", "void", "ρ"))
        );
    }

    @Test
    void spellsDataFormaAsMarkerOfItself(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the data forma on the way must stand as a marker of its own carrier, but it doesnt",
            new Applied(
                AppliedTest.data(), "Φ.number.minus",
                new Formas(Collections.emptyMap(), Collections.emptyMap()),
                new Symbols(temp.resolve("s.tsv"))
            ).phi(),
            Matchers.startsWith("Φ.number( φ ↦ Φ.bytes( φ ↦ ⟦ λ ⤍ S2 ⟧ ) ).minus( x ↦ ")
        );
    }

    private static Document doc() {
        return new Xnav(
            String.join(
                "",
                "<object><o loc='Φ.foo' name='foo'><o base='∅' name='n' loc='Φ.foo.n'/>",
                "<o loc='Φ.foo.f' name='f'><o base='∅' name='x'/><o base='∅' name='ρ'/>",
                "<o base='ξ.x' name='φ'/></o></o></object>"
            )
        ).element("object").node().getOwnerDocument();
    }

    private static Document data() {
        return new Xnav(
            String.join(
                "",
                "<object><o loc='Φ.number' name='number'><o base='∅' name='φ'/>",
                "<o loc='Φ.number.minus' name='minus'><o base='∅' name='ρ'/><o base='∅' name='x'/>",
                "<o base='ξ.ρ.plus' name='φ'><o as='α0' base='ξ.x'/></o></o></o></object>"
            )
        ).element("object").node().getOwnerDocument();
    }
}
