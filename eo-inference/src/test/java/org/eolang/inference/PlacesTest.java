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
 * Test case for {@link Places}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class PlacesTest {

    @Test
    void startsChainAtDatum(@Mktmp final Path temp) throws IOException {
        final State state = PlacesTest.settled(temp);
        MatcherAssert.assertThat(
            "a chain that starts at a datum must answer the datum, but it didnt",
            state.places.evaluate("Φ.garden.φ.α0.α0", PlacesTest.garden(state)),
            Matchers.equalTo(new Ids(Values.DATA))
        );
    }

    @Test
    void startsBareXiAtCopyAround(@Mktmp final Path temp) throws IOException {
        final State state = PlacesTest.settled(temp);
        final int bed = PlacesTest.bed(state, PlacesTest.garden(state));
        MatcherAssert.assertThat(
            "a bare ξ inside bed must be the copy of bed, but it wasnt",
            state.places.evaluate("Φ.garden.bed.φ.ρ", bed),
            Matchers.equalTo(new Ids(bed))
        );
    }

    @Test
    void readsCaretOffCopyAsItsRho(@Mktmp final Path temp) throws IOException {
        final State state = PlacesTest.settled(temp);
        final int garden = PlacesTest.garden(state);
        MatcherAssert.assertThat(
            "^ read inside bed must be the copy of garden around it, but it wasnt",
            state.places.evaluate("Φ.garden.bed.φ", PlacesTest.bed(state, garden)),
            Matchers.equalTo(new Ids(garden))
        );
    }

    @Test
    void makesCopyOfFormationWrittenAtPlace(@Mktmp final Path temp) throws IOException {
        final State state = PlacesTest.settled(temp);
        final int garden = PlacesTest.garden(state);
        MatcherAssert.assertThat(
            "a formation at a place must answer its copy born there, but it didnt",
            state.places.evaluate("Φ.garden.bed", garden),
            Matchers.equalTo(new Ids(PlacesTest.bed(state, garden)))
        );
    }

    private static int garden(final State state) {
        return state.values.intern(new Value('O', "Φ.garden", "Φ.garden", Formation.RHO, "", -1, 0));
    }

    private static int bed(final State state, final int garden) {
        final int bed = state.values.intern(
            new Value('O', "Φ.garden.bed", "Φ.garden.bed", Formation.RHO, "", -1, 0)
        );
        if (state.rows.row(bed, "ρ").isEmpty() || !state.rows.row(bed, "ρ").contains(garden)) {
            throw new IllegalStateException("the bed of the garden is not inside it");
        }
        return bed;
    }

    private static State settled(final Path temp) throws IOException {
        return new Sources(
            temp,
            "garden.eo", "[] > garden\n  5 > @\n  [] > bed\n    ^ > @\n"
        ).settled();
    }
}
