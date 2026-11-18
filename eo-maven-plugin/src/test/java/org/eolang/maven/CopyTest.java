/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.security.SecureRandom;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.eolang.inference.Annotation;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link Copy}.
 *
 * @since 0.76.0
 */
final class CopyTest {

    @Test
    void readsArgumentOfVoidInCopyItsCallIsWrittenIn() {
        final String host = String.format("Φ.host%d(Φ.number)", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "the argument in a void must be read in the copy its call is written in, but it isnt",
            new Copy(new Annotation("Φ.status")).applied(
                "Φ.app.φ", new Copy(new Annotation(host))
            ).argument(
                "cde",
                new Calls(
                    Map.of("Φ.status", List.of("cde")),
                    Map.of("Φ.app.φ", Map.of(0, "Φ.app.φ.α0")),
                    Map.of(),
                    Map.of()
                )
            ),
            Matchers.equalTo(
                Optional.of(Map.entry("Φ.app.φ.α0", new Copy(new Annotation(host))))
            )
        );
    }

    @Test
    void keepsOnlyFormationOfHostMadeByCall() {
        MatcherAssert.assertThat(
            "a copy made by a call must be kept as its formation alone when another call is written in it, but it isnt",
            new Copy(new Annotation("Φ.pair")).applied(
                "Φ.app.φ",
                new Copy(new Annotation("Φ.box")).applied(
                    "Φ.app.box", new Copy(new Annotation("Φ.app"))
                )
            ).argument(
                "left",
                new Calls(
                    Map.of("Φ.pair", List.of("left")),
                    Map.of("Φ.app.φ", Map.of(0, "Φ.app.φ.α0")),
                    Map.of(),
                    Map.of()
                )
            ).get().getValue(),
            Matchers.equalTo(new Copy(new Annotation("Φ.box")))
        );
    }

    @Test
    void equalsCopyThatSameCallMakesInSameHost() {
        final String place = String.format("Φ.app.x%d", new SecureRandom().nextInt(10_000));
        MatcherAssert.assertThat(
            "two copies that one call makes in one host must be equal, but they werent",
            new Copy(new Annotation("Φ.pair")).applied(
                place, new Copy(new Annotation("Φ.app(Φ.number)"))
            ),
            Matchers.equalTo(
                new Copy(new Annotation("Φ.pair")).applied(
                    place, new Copy(new Annotation("Φ.app(Φ.number)"))
                )
            )
        );
    }

    @Test
    void agreesOnFormationThatCopiesOfDifferentCallsShare() {
        final Copy outer = new Copy(new Annotation("Φ.app"));
        MatcherAssert.assertThat(
            "copies of one formation made by different calls must agree on the formation, but they dont",
            Copy.agreed(
                List.of(
                    Optional.of(new Copy(new Annotation("Φ.number")).applied("Φ.app.α0", outer)),
                    Optional.of(new Copy(new Annotation("Φ.number")).applied("Φ.app.α1", outer))
                ),
                CopyTest.calls()
            ),
            Matchers.equalTo(Optional.of(new Copy(new Annotation("Φ.number"))))
        );
    }

    @Test
    void agreesOnNothingWhenOneCopyIsUnknown() {
        MatcherAssert.assertThat(
            "copies must agree on nothing when one of them is unknown, but they agree",
            Copy.agreed(
                List.of(Optional.of(new Copy(new Annotation("Φ.number"))), Optional.empty()),
                CopyTest.calls()
            ),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void blursFormationsWithSameVoids() {
        MatcherAssert.assertThat(
            "copies of formations with the same voids must agree on a blurred copy of them, but they dont",
            Copy.agreed(
                List.of(
                    Optional.of(new Copy(new Annotation("Φ.true"))),
                    Optional.of(new Copy(List.of("Φ.true", "Φ.false")))
                ),
                CopyTest.calls()
            ),
            Matchers.equalTo(Optional.of(new Copy(List.of("Φ.false", "Φ.true"))))
        );
    }

    @Test
    void keepsCallThatMadeAllCopies() {
        final Copy outer = new Copy(new Annotation("Φ.app"));
        MatcherAssert.assertThat(
            "copies made by one call must agree on a copy made by it, but they dont",
            Copy.agreed(
                List.of(
                    Optional.of(new Copy(new Annotation("Φ.true")).applied("Φ.app.φ", outer)),
                    Optional.of(new Copy(new Annotation("Φ.false")).applied("Φ.app.φ", outer))
                ),
                CopyTest.calls()
            ),
            Matchers.equalTo(
                Optional.of(new Copy(List.of("Φ.true", "Φ.false")).applied("Φ.app.φ", outer))
            )
        );
    }

    @Test
    void agreesOnNothingWhenVoidsDiffer() {
        MatcherAssert.assertThat(
            "copies of formations with different voids must agree on nothing, but they agree",
            Copy.agreed(
                List.of(
                    Optional.of(new Copy(new Annotation("Φ.tuple.eq"))),
                    Optional.of(new Copy(List.of("Φ.bytes.eq", "Φ.bytes.gt")))
                ),
                CopyTest.calls()
            ),
            Matchers.equalTo(Optional.empty())
        );
    }

    @Test
    void givesOneSharpCandidatePerFormationWithSameCall() {
        final Copy outer = new Copy(new Annotation("Φ.app"));
        MatcherAssert.assertThat(
            "a blurred copy must give a sharp copy of each formation, made by its call, but it doesnt",
            new Copy(List.of("Φ.true", "Φ.false")).applied("Φ.app.φ", outer).candidates(),
            Matchers.contains(
                new Copy(new Annotation("Φ.false")).applied("Φ.app.φ", outer),
                new Copy(new Annotation("Φ.true")).applied("Φ.app.φ", outer)
            )
        );
    }

    @Test
    void isNotSoleWhenBlurred() {
        MatcherAssert.assertThat(
            "a copy of several formations must not be sole, but it is",
            new Copy(List.of("Φ.true", "Φ.false")).sole(),
            Matchers.is(false)
        );
    }

    private static Calls calls() {
        return new Calls(
            Map.of(
                "Φ.true", List.of("left", "right"),
                "Φ.false", List.of("left", "right"),
                "Φ.tuple.eq", List.of("other"),
                "Φ.bytes.eq", List.of("b"),
                "Φ.bytes.gt", List.of("b")
            ),
            Map.of(),
            Map.of(),
            Map.of()
        );
    }
}
