/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Suffix}.
 * @since 0.1
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.UnnecessaryLocalRule"})
final class SuffixTest {

    @Test
    void reportsNoneForEmptyTail() {
        MatcherAssert.assertThat(
            "an empty tail must yield Form.NONE so the line is unnamed",
            new Suffix("", new Span("[]", 1), 2).form(),
            Matchers.equalTo(Suffix.Form.NONE)
        );
    }

    @Test
    void reportsNoneWhenTailHasOnlyWhitespace() {
        MatcherAssert.assertThat(
            "a whitespace-only tail must still yield Form.NONE",
            new Suffix("   ", new Span("[]   ", 1), 2).form(),
            Matchers.equalTo(Suffix.Form.NONE)
        );
    }

    @Test
    void parsesExplicitName() {
        final Suffix suffix = new Suffix(" > foo", new Span("[] > foo", 1), 2);
        MatcherAssert.assertThat(
            "`> name` must yield Form.NAME with the parsed identifier",
            suffix.form(),
            Matchers.equalTo(Suffix.Form.NAME)
        );
    }

    @Test
    void parsesExplicitNameLabel() {
        MatcherAssert.assertThat(
            "the label() must round-trip the identifier between `>` and the next terminator",
            new Suffix(" > foo", new Span("[] > foo", 1), 2).label(),
            Matchers.equalTo("foo")
        );
    }

    @Test
    void parsesAutoName() {
        MatcherAssert.assertThat(
            "`>>` must yield Form.AUTO with no label",
            new Suffix(" >>", new Span("[] >>", 1), 2).form(),
            Matchers.equalTo(Suffix.Form.AUTO)
        );
    }

    @Test
    void parsesPlusGreaterAttribute() {
        MatcherAssert.assertThat(
            "`+> name` must yield Form.TEST with the parsed name",
            new Suffix(" +> tests-add", new Span("[] +> tests-add", 1), 2).form(),
            Matchers.equalTo(Suffix.Form.TEST)
        );
    }

    @Test
    void carriesPlusGreaterName() {
        MatcherAssert.assertThat(
            "a test suffix's label() must be the identifier after `+>`",
            new Suffix(" +> tests-add", new Span("[] +> tests-add", 1), 2).label(),
            Matchers.equalTo("tests-add")
        );
    }

    @Test
    void detectsConstMarkerOnNamedSuffix() {
        MatcherAssert.assertThat(
            "`> name!` must report constant() == true",
            new Suffix(" > foo!", new Span("[] > foo!", 1), 2).constant(),
            Matchers.is(true)
        );
    }

    @Test
    void detectsConstMarkerOnAutoSuffix() {
        MatcherAssert.assertThat(
            "`>>!` must report constant() == true (auto-named const is syntactically valid)",
            new Suffix(" >>!", new Span("[] >>!", 1), 2).constant(),
            Matchers.is(true)
        );
    }

    @Test
    void parsesAtomSignature() {
        MatcherAssert.assertThat(
            "`> name /sig` must record the atom signature as sig()",
            new Suffix(" > foo /number", new Span("[] > foo /number", 1), 2).sig(),
            Matchers.equalTo("number")
        );
    }

    @Test
    void marksAtomFlagWhenSigPresent() {
        MatcherAssert.assertThat(
            "atom() must report true once a /sig has been read",
            new Suffix(" > foo /number", new Span("[] > foo /number", 1), 2).atom(),
            Matchers.is(true)
        );
    }

    @Test
    void leavesAtomFlagFalseForBareName() {
        MatcherAssert.assertThat(
            "a bare `> name` cannot be an atom",
            new Suffix(" > foo", new Span("[] > foo", 1), 2).atom(),
            Matchers.is(false)
        );
    }

    @Test
    void promotesRootedSignatureToPhi() {
        MatcherAssert.assertThat(
            "a /Q.x signature must be promoted to /Φ.x per R-3.10.11 / R-9.3",
            new Suffix(
                " > foo /Q.org.eolang.number",
                new Span("[] > foo /Q.org.eolang.number", 1), 2
            ).sig(),
            Matchers.equalTo("Φ.org.eolang.number")
        );
    }

    @Test
    void rejectsConstCombinedWithAtomSignature() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Suffix(
                " > foo! /number",
                new Span("[] > foo! /number", 1), 2
            ),
            "`> name! /sig` must be rejected per R-3.10.3"
        );
    }

    @Test
    void rejectsAutoCombinedWithAtomSignature() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Suffix(
                " >> /number",
                new Span("[] >> /number", 1), 2
            ),
            "`>> /sig` must be rejected per R-3.10.2"
        );
    }

    @Test
    void rejectsBareQSignature() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Suffix(
                " > foo /Q",
                new Span("[] > foo /Q", 1), 2
            ),
            "a bare /Q signature must be rejected per R-3.10.10"
        );
    }

    @Test
    void rejectsEmptySignature() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Suffix(
                " > foo /",
                new Span("[] > foo /", 1), 2
            ),
            "a bare `/` with no signature must be rejected"
        );
    }

    @Test
    void rejectsPlusGreaterWithPhi() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Suffix(
                " +> @",
                new Span("[] +> @", 1), 2
            ),
            "`+> @` must be rejected per R-6.3.5"
        );
    }

    @Test
    void rejectsTrailingGarbageThatIsNotASuffixMarker() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Suffix("x", new Span("5x", 1), 1),
            "trailing content after a head that is not `>`, `>>`, or `+>` must be rejected,"
                .concat(" not silently dropped")
        );
    }

    @Test
    void rejectsNamedSuffixWithoutName() {
        Assertions.assertThrows(
            ParseError.class,
            () -> new Suffix(" > ", new Span("[] > ", 1), 2),
            "`> ` with no name must be rejected"
        );
    }

    @Test
    void reportsAutoFlagOnDoubleArrow() {
        MatcherAssert.assertThat(
            "auto() must report true for `>>` suffix",
            new Suffix(" >>", new Span("[] >>", 1), 2).auto(),
            Matchers.is(true)
        );
    }

    @Test
    void reportsTrueForPlusGreaterSuffix() {
        MatcherAssert.assertThat(
            "test() must report true for `+>` suffix",
            new Suffix(" +> tests-add", new Span("[] +> tests-add", 1), 2).test(),
            Matchers.is(true)
        );
    }

    @Test
    void reportsPresentForAnyNonEmptyForm() {
        MatcherAssert.assertThat(
            "present() must report true for any non-NONE form",
            new Suffix(" > foo", new Span("[] > foo", 1), 2).present(),
            Matchers.is(true)
        );
    }

    @Test
    void reportsAbsentForEmpty() {
        MatcherAssert.assertThat(
            "present() must report false for an empty tail",
            new Suffix("", new Span("[]", 1), 2).present(),
            Matchers.is(false)
        );
    }

    @Test
    void mapsAtAttributeNameToPhi() {
        MatcherAssert.assertThat(
            "an explicit `> @` suffix must surface as the φ attribute per R-9.3.1",
            new Suffix(" > @", new Span("foo > @", 1), 3).attribute(1, 0),
            Matchers.equalTo("φ")
        );
    }

    @Test
    void preservesPlainNameAttribute() {
        final String tail = " > stamp";
        MatcherAssert.assertThat(
            "a regular `> name` suffix must pass through to the attribute unchanged",
            new Suffix(tail, new Span("[]".concat(tail), 1), 2).attribute(1, 0),
            Matchers.equalTo("stamp")
        );
    }

    @Test
    void prefixesPlusFormAttribute() {
        MatcherAssert.assertThat(
            "a `+> name` test suffix must surface as `+name` in the attribute",
            new Suffix(" +> ready", new Span("[] +> ready", 1), 2).attribute(1, 0),
            Matchers.equalTo("+ready")
        );
    }

    @Test
    void generatesAutoNameAttribute() {
        MatcherAssert.assertThat(
            "an `>>` auto suffix must produce the canonical `a🌵<line>-<indent>` placeholder",
            new Suffix(" >>", new Span("[] >>", 1), 2).attribute(7, 4),
            Matchers.equalTo("a🌵7-4")
        );
    }

    @Test
    void omitsAttributeWhenSuffixAbsent() {
        MatcherAssert.assertThat(
            "with no suffix, attribute() must return null so callers can skip @name emission",
            new Suffix("", new Span("foo", 1), 3).attribute(1, 0),
            Matchers.nullValue()
        );
    }
}
