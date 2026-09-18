/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Bindings}.
 *
 * @since 0.63
 */
final class BindingsTest {

    @Test
    void findsAnAttributeByItsName() {
        final Map<String, Attribute> attrs = new Bindings();
        final Attribute attr = new AtVoid("weight");
        attrs.put("weight", attr);
        MatcherAssert.assertThat(
            "an attribute put under a name must be found by it, but it wasnt",
            attrs.get("weight"),
            Matchers.sameInstance(attr)
        );
    }

    @Test
    void findsNothingUnderAnUnknownName() {
        final Map<String, Attribute> attrs = new Bindings();
        attrs.put("height", new AtVoid("height"));
        MatcherAssert.assertThat(
            "a name nobody put must be found by nobody, but something was found",
            attrs.containsKey("width"),
            Matchers.is(false)
        );
    }

    @Test
    void replacesWhatSitsUnderTheSameName() {
        final Map<String, Attribute> attrs = new Bindings();
        final Attribute second = new AtVoid("second");
        attrs.put("x", new AtVoid("first"));
        attrs.put("x", second);
        MatcherAssert.assertThat(
            "putting twice under one name must leave the later attribute, but it left the earlier",
            attrs.get("x"),
            Matchers.sameInstance(second)
        );
    }

    @Test
    void countsOneNameOnceHoweverOftenItIsPut() {
        final Map<String, Attribute> attrs = new Bindings();
        attrs.put("only", new AtVoid("first"));
        attrs.put("only", new AtVoid("second"));
        MatcherAssert.assertThat(
            "putting twice under one name must count as one attribute, but it counted two",
            attrs.size(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void keepsTheOrderInWhichNamesArrived() {
        final Map<String, Attribute> attrs = new Bindings();
        final List<String> names = new ArrayList<>(3);
        for (final String name : new String[] {"zebra", "ant", "moose"}) {
            attrs.put(name, new AtVoid(name));
        }
        for (final Map.Entry<String, Attribute> ent : attrs.entrySet()) {
            names.add(ent.getKey());
        }
        MatcherAssert.assertThat(
            "attributes must be listed in the order they arrived, but they were reordered",
            names,
            Matchers.contains("zebra", "ant", "moose")
        );
    }

    @Test
    void makesRoomForMoreThanItStartedWith() {
        final Map<String, Attribute> attrs = new Bindings();
        for (int idx = 0; idx < 37; ++idx) {
            attrs.put(String.format("attr%d", idx), new AtVoid("void"));
        }
        MatcherAssert.assertThat(
            "an attribute put after the room ran out must still be found, but it was lost",
            attrs.containsKey("attr36"),
            Matchers.is(true)
        );
    }

    @Test
    void holdsNothingUntilSomethingIsPut() {
        MatcherAssert.assertThat(
            "a fresh set of attributes must hold none, but it held some",
            new Bindings().size(),
            Matchers.equalTo(0)
        );
    }
}
