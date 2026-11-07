/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import java.util.stream.Collectors;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Annotation}.
 *
 * @since 0.76.0
 */
final class AnnotationTest {

    @Test
    void givesHeadOfTypeWithoutItsBrackets() {
        MatcherAssert.assertThat(
            "the head of a type must leave its brackets out, but it kept them",
            new Annotation("Φ.posix.status-return(Φ.number,Φ.string)").head(),
            Matchers.equalTo("Φ.posix.status-return")
        );
    }

    @Test
    void splitsBracketsIntoMembersInOrder() {
        MatcherAssert.assertThat(
            "the brackets must split into their members in order, but they didnt",
            new Annotation("Φ.a(Φ.b(Φ.c,Φ.d4),Φ.f-x,B)").members().stream()
                .map(Annotation::toString).collect(Collectors.toList()),
            Matchers.contains("Φ.b(Φ.c,Φ.d4)", "Φ.f-x", "B")
        );
    }

    @Test
    void givesNoMembersToTypeWithoutBrackets() {
        MatcherAssert.assertThat(
            "a type without brackets must have no members, but it had some",
            new Annotation("Φ.tw.ig").members(),
            Matchers.empty()
        );
    }

    @Test
    void dropsMarkOfMaybeTermination() {
        MatcherAssert.assertThat(
            "the mark of a void that may hold a termination must be dropped, but it stayed",
            new Annotation("Φ.pair(Φ.number,Φ.string)?"),
            Matchers.hasToString("Φ.pair(Φ.number,Φ.string)")
        );
    }

    @Test
    void tellsTypeVariable() {
        MatcherAssert.assertThat(
            "a letter from A to F must be a type variable, but it wasnt",
            new Annotation("E?").variable(),
            Matchers.is(true)
        );
    }

    @Test
    void tellsPathIsNoTypeVariable() {
        MatcherAssert.assertThat(
            "a path must not be a type variable, but it was",
            new Annotation("Φ.C").variable(),
            Matchers.is(false)
        );
    }
}
