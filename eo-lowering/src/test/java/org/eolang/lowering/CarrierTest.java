/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.github.lombrozo.xnav.Xnav;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Carrier}.
 * @since 0.76.0
 */
final class CarrierTest {

    @Test
    void readsNumberThroughItsBytes() {
        MatcherAssert.assertThat(
            "a number must be read as its forma and the bytes it wraps, but it isnt",
            new Carrier(
                new Xnav(
                    String.join(
                        "",
                        "<o base='Φ.number'>",
                        "<o as='α0' base='Φ.bytes'><o as='α0'>40-00-00-00-00-00-00-00</o></o>",
                        "</o>"
                    )
                ).element("o")
            ).literal().key(),
            Matchers.equalTo("number:40-00-00-00-00-00-00-00")
        );
    }

    @Test
    void readsBoolWithoutDatum() {
        MatcherAssert.assertThat(
            "a bool must be its own datum, but it isnt",
            new Carrier(new Xnav("<o base='Φ.false'/>").element("o")).literal().key(),
            Matchers.equalTo("bool:00-")
        );
    }

    @Test
    void readsBytesDirectly() {
        MatcherAssert.assertThat(
            "bytes must wrap their datum directly, but they dont",
            new Carrier(
                new Xnav("<o base='Φ.bytes'><o as='α0'>01-02</o></o>").element("o")
            ).literal().key(),
            Matchers.equalTo("bytes:01-02")
        );
    }

    @Test
    void refusesNumberWithoutBytes() {
        Assertions.assertThrows(
            IllegalStateException.class,
            new Carrier(
                new Xnav("<o base='Φ.number'><o as='α0'>40-00-00-00-00-00-00-00</o></o>")
                    .element("o")
            )::literal,
            "a number wrapping no bytes carrier is malformed, but it was read"
        );
    }

    @Test
    void refusesObjectThatCarriesNoDatum() {
        MatcherAssert.assertThat(
            "an object of the universe that is no datum must be refused, but it wasnt",
            Assertions.assertThrows(
                IllegalStateException.class,
                new Carrier(
                    new Xnav("<o base='Φ.dataized'><o base='ξ.x'/></o>").element("o")
                )::literal,
                "a dataized object carries no datum, but it was read"
            ).getMessage(),
            Matchers.containsString("cannot stand in a lowered fragment")
        );
    }
}
