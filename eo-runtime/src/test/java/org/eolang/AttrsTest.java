/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Attrs}.
 *
 * @since 0.59
 */
final class AttrsTest {

    @Test
    void keepsTheOrderOfTheEntriesItWasGiven() {
        MatcherAssert.assertThat(
            "the attributes must come out in the order they were given, but they didnt",
            new Attrs(
                new Attr("x", new AtVoid("x")),
                new Attr("y", new AtVoid("y"))
            ).keySet(),
            Matchers.contains("x", "y")
        );
    }

    @Test
    void ignoresLaterChangesToTheArrayItWasGiven() {
        final Attr[] given = {new Attr("x", new AtVoid("x"))};
        final Attrs attrs = new Attrs(given);
        given[0] = new Attr("y", new AtVoid("y"));
        MatcherAssert.assertThat(
            "a change to the array must not reach the attributes, but it did",
            attrs.keySet(),
            Matchers.contains("x")
        );
    }

    @Test
    void refusesToChangeTheEntryItWasGiven() {
        final Attribute attr = new AtVoid("x");
        final Attr entry = new Attr("x", attr);
        final Attrs attrs = new Attrs(entry);
        Assertions.assertThrows(
            UnsupportedOperationException.class,
            () -> entry.setValue(new AtVoid("y")),
            "an entry already given away must not be changeable, but it was"
        );
        MatcherAssert.assertThat(
            "the attributes must still hold what the entry was made with, but they didnt",
            attrs.get("x"),
            Matchers.sameInstance(attr)
        );
    }

    @Test
    void countsTheEntriesWithoutReadingThemAll() {
        MatcherAssert.assertThat(
            "the size must count every entry given, but it didnt",
            new Attrs(
                new Attr("x", new AtVoid("x")),
                new Attr("y", new AtVoid("y"))
            ).size(),
            Matchers.equalTo(2)
        );
    }
}
