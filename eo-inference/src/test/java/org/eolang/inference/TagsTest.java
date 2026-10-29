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
            TagsTest.tags(temp).tagged("", "Φ.tuple.with", "Φ.shop.φ"),
            Matchers.equalTo("Φ.shop.φ")
        );
    }

    @Test
    void givesNoTagToOtherCodeMadeByCodeWithNoTag(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy of other code made by code with no tag must take no tag, but it took one",
            TagsTest.tags(temp).tagged("", "Φ.box", "Φ.shop.φ"),
            Matchers.emptyString()
        );
    }

    @Test
    void passesTagOnToCopyOfAnyFormation(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy made by tagged code must take its tag, but it didnt",
            TagsTest.tags(temp).tagged("Φ.shop.φ", "Φ.box", "Φ.tuple.with.φ"),
            Matchers.equalTo("Φ.shop.φ")
        );
    }

    @Test
    void growsTagWhereListCodeCallsAnotherPackage(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy of tuple made by map code must add the place to the tag, but it didnt",
            TagsTest.tags(temp).tagged("Φ.shop.φ", "Φ.tuple.with", "Φ.map.put.φ"),
            Matchers.equalTo("Φ.shop.φ/Φ.map.put.φ")
        );
    }

    @Test
    void addsNoPlaceOfPackageTwice(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a package that already added a place must add none again, but it did",
            TagsTest.tags(temp).tagged("Φ.shop.φ/Φ.map.put.φ", "Φ.tuple.with", "Φ.map.get.φ"),
            Matchers.equalTo("Φ.shop.φ/Φ.map.put.φ")
        );
    }

    @Test
    void givesNoTagToFormationOfScalarPackage(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy of a number method must take no tag, but it took one",
            TagsTest.tags(temp).tagged("Φ.shop.φ", "Φ.number.plus", "Φ.tuple.with.φ"),
            Matchers.emptyString()
        );
    }

    @Test
    void givesNoTagToObjectThatPassesItsVoidOn(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a copy of an object whose @ is its void must take no tag, but it took one",
            TagsTest.tags(temp).tagged("Φ.shop.φ", "Φ.same", "Φ.tuple.with.φ"),
            Matchers.emptyString()
        );
    }

    @Test
    void findsNoPackageInsideTest(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "a name inside a test must have no package, but it had one",
            TagsTest.tags(temp).pkg("Φ.tuple.p🌵can-add.φ"),
            Matchers.emptyString()
        );
    }

    private static Tags tags(final Path temp) throws IOException {
        return new State(
            new Sources(
                temp,
                "tuple.eo", "[item rest] > tuple\n  [x] > with\n    tuple x ^ > @\n",
                "map.eo", "[pairs] > map\n  [key] > put\n    ^.pairs.with key > @\n",
                "box.eo", "[item] > box\n  item > @\n  1 > size\n",
                "same.eo", "[x] > same\n  x > @\n",
                "number.eo", "[@] > number\n  [] > plus /Q.number\n    ? > ^\n    ? > b\n"
            ).program(),
            new Names()
        ).tags;
    }
}
