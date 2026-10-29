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
 * Test case for {@link Tags}.
 *
 * @since 0.76.0
 */
@ExtendWith(MktmpResolver.class)
final class TagsTest {

    @Test
    void startsTagWhereCodeWithNoTagMakesListCode(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy of list code made by code with no tag must be tagged with the place, but it wasnt",
            TagsTest.tags(temp).tagged(new Site("Φ.shop.φ", ""), "Φ.tuple.with"),
            Matchers.equalTo("Φ.shop.φ")
        );
    }

    @Test
    void givesNoTagToOtherCodeMadeByCodeWithNoTag(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy of other code made by code with no tag must take no tag, but it took one",
            TagsTest.tags(temp).tagged(new Site("Φ.shop.φ", ""), "Φ.box"),
            Matchers.emptyString()
        );
    }

    @Test
    void passesTagOnToCopyOfAnyFormation(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy made by tagged code must take its tag, but it didnt",
            TagsTest.tags(temp).tagged(new Site("Φ.tuple.with.φ", "Φ.shop.φ"), "Φ.box"),
            Matchers.equalTo("Φ.shop.φ")
        );
    }

    @Test
    void growsTagWhereListCodeCallsAnotherPackage(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy of tuple made by map code must add the place to the tag, but it didnt",
            TagsTest.tags(temp).tagged(new Site("Φ.map.put.φ", "Φ.shop.φ"), "Φ.tuple.with"),
            Matchers.equalTo("Φ.shop.φ/Φ.map.put.φ")
        );
    }

    @Test
    void addsNoPlaceOfPackageTwice(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a package that already added a place must add none again, but it did",
            TagsTest.tags(temp).tagged(
                new Site("Φ.map.get.φ", "Φ.shop.φ/Φ.map.put.φ"), "Φ.tuple.with"
            ),
            Matchers.equalTo("Φ.shop.φ/Φ.map.put.φ")
        );
    }

    @Test
    void givesNoTagToFormationOfScalarPackage(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy of a number method must take no tag, but it took one",
            TagsTest.tags(temp).tagged(new Site("Φ.tuple.with.φ", "Φ.shop.φ"), "Φ.number.plus"),
            Matchers.emptyString()
        );
    }

    @Test
    void givesNoTagToObjectThatPassesItsVoidOn(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy of an object whose @ is its void must take no tag, but it took one",
            TagsTest.tags(temp).tagged(new Site("Φ.tuple.with.φ", "Φ.shop.φ"), "Φ.same"),
            Matchers.emptyString()
        );
    }

    @Test
    void findsNoPackageInsideCheck(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a name inside a test must have no package, but it had one",
            TagsTest.tags(temp).pkg("Φ.tuple.p🌵can-add.φ"),
            Matchers.emptyString()
        );
    }

    private static Tags tags(final Path temp) throws IOException {
        return new State(
            new Sources(temp).with(
                "tuple.eo",
                "[item rest] > tuple",
                "  [x] > with",
                "    tuple x ^ > @"
            ).with(
                "map.eo",
                "[pairs] > map",
                "  [key] > put",
                "    ^.pairs.with key > @"
            ).with(
                "box.eo",
                "[item] > box",
                "  item > @",
                "  1 > size"
            ).with(
                "same.eo",
                "[x] > same",
                "  x > @"
            ).with(
                "number.eo",
                "[@] > number",
                "  [] > plus /Q.number",
                "    ? > ^",
                "    ? > b"
            ).program(),
            new Names()
        ).tags();
    }
}
