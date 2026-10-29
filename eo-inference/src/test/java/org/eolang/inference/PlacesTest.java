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
        final World world = PlacesTest.world(temp);
        MatcherAssert.assertThat(
            "a chain that starts at a datum must answer the datum, but it didnt",
            new Settled(world).state().evaluate("Φ.garden.φ.α0.α0", PlacesTest.garden(world)),
            Matchers.equalTo(new Ids(Values.DATA))
        );
    }

    @Test
    void startsBareXiAtCopyAround(@Mktmp final Path temp) throws IOException {
        final World world = PlacesTest.world(temp);
        final State state = new Settled(world).state();
        final int bed = PlacesTest.bed(state, world, PlacesTest.garden(world));
        MatcherAssert.assertThat(
            "a bare ξ inside bed must be the copy of bed, but it wasnt",
            state.evaluate("Φ.garden.bed.φ.ρ", bed),
            Matchers.equalTo(new Ids(bed))
        );
    }

    @Test
    void readsCaretOffCopyAsItsRho(@Mktmp final Path temp) throws IOException {
        final World world = PlacesTest.world(temp);
        final State state = new Settled(world).state();
        final int garden = PlacesTest.garden(world);
        MatcherAssert.assertThat(
            "^ read inside bed must be the copy of garden around it, but it wasnt",
            state.evaluate("Φ.garden.bed.φ", PlacesTest.bed(state, world, garden)),
            Matchers.equalTo(new Ids(garden))
        );
    }

    @Test
    void makesCopyOfFormationWrittenAtPlace(@Mktmp final Path temp) throws IOException {
        final World world = PlacesTest.world(temp);
        final State state = new Settled(world).state();
        final int garden = PlacesTest.garden(world);
        MatcherAssert.assertThat(
            "a formation at a place must answer its copy born there, but it didnt",
            state.evaluate("Φ.garden.bed", garden),
            Matchers.equalTo(new Ids(PlacesTest.bed(state, world, garden)))
        );
    }

    private static int garden(final World world) {
        return world.intern(
            new Value('O', "Φ.garden", "Φ.garden", Formation.RHO, "", -1, 0)
        );
    }

    private static int bed(final State state, final World world, final int garden) {
        final int bed = world.intern(
            new Value('O', "Φ.garden.bed", "Φ.garden.bed", Formation.RHO, "", -1, 0)
        );
        if (state.row(bed, "ρ").isEmpty() || !state.row(bed, "ρ").contains(garden)) {
            throw new IllegalStateException("the bed of the garden is not inside it");
        }
        return bed;
    }

    private static World world(final Path temp) throws IOException {
        return new Sources(temp)
            .with("garden.eo", "[] > garden", "  5 > @", "  [] > bed", "    ^ > @")
            .world();
    }
}
