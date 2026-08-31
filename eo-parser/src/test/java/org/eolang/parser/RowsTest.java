/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.ArrayList;
import java.util.List;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Rows}.
 * @since 0.50
 */
final class RowsTest {

    @Test
    void underlinesTheOffendingLine() {
        MatcherAssert.assertThat(
            "the line at the given number is not quoted with a caret under its position",
            new Rows(List.of(new Span("привет мир", 1), new Span("второй", 2)))
                .underlined(1, 7, "боль"),
            Matchers.equalTo(String.format("[1:7] error: 'боль'%nпривет мир%n       ^"))
        );
    }

    @Test
    void keepsMessageBareWhenLineIsUnknown() {
        MatcherAssert.assertThat(
            "a number past the end of the source is not answered with the bare message",
            new Rows(List.of(new Span("", 1))).underlined(9, 2, "ошибка"),
            Matchers.equalTo("[9:2] error: 'ошибка'")
        );
    }

    @Test
    void ignoresLineAppendedAfterConstruction() {
        final List<Span> source = new ArrayList<>(1);
        source.add(new Span("первый", 1));
        final Rows lines = new Rows(source);
        source.add(new Span("второй", 2));
        MatcherAssert.assertThat(
            "a line appended to the caller's list after construction is not left out",
            lines.underlined(2, 0, "поздно"),
            Matchers.equalTo("[2:0] error: 'поздно'")
        );
    }

    @Test
    void keepsLineRemovedAfterConstruction() {
        final List<Span> source = new ArrayList<>(2);
        source.add(new Span("первый", 1));
        source.add(new Span("второй", 2));
        final Rows lines = new Rows(source);
        source.remove(1);
        MatcherAssert.assertThat(
            "a line removed from the caller's list after construction is not still quoted",
            lines.underlined(2, 0, "рано"),
            Matchers.equalTo(String.format("[2:0] error: 'рано'%nвторой%n^"))
        );
    }

    @Test
    void keepsLinesWhenCallerListIsCleared() {
        final List<Span> source = new ArrayList<>(1);
        source.add(new Span("осталась", 1));
        final Rows lines = new Rows(source);
        source.clear();
        MatcherAssert.assertThat(
            "a line is not quoted after the caller's list was emptied",
            lines.underlined(1, 0, "пусто"),
            Matchers.equalTo(String.format("[1:0] error: 'пусто'%nосталась%n^"))
        );
    }

    @Test
    void ignoresLineReplacedAfterConstruction() {
        final List<Span> source = new ArrayList<>(1);
        source.add(new Span("старая", 1));
        final Rows lines = new Rows(source);
        source.set(0, new Span("новая", 1));
        MatcherAssert.assertThat(
            "the line replaced in the caller's list after construction is not the old one",
            lines.underlined(1, 0, "замена"),
            Matchers.equalTo(String.format("[1:0] error: 'замена'%nстарая%n^"))
        );
    }

    @Test
    void underlinesEmptyLineAtValidNumber() {
        MatcherAssert.assertThat(
            "a real empty line is not told apart from a number outside the source",
            new Rows(List.of(new Span("", 1))).underlined(1, 0, "пусто"),
            Matchers.equalTo(String.format("[1:0] error: 'пусто'%n%n"))
        );
    }

    @Test
    void keepsMessageBareWhenNumberIsZero() {
        MatcherAssert.assertThat(
            "a number below the first line is not answered with the bare message",
            new Rows(List.of(new Span("привет", 1))).underlined(0, 3, "ноль"),
            Matchers.equalTo("[0:3] error: 'ноль'")
        );
    }

    @Test
    void quotesLineWithoutCaretWhenPositionIsPastItsEnd() {
        MatcherAssert.assertThat(
            "a position past the end of the line is not quoted with no caret beneath it",
            new Rows(List.of(new Span("привет", 1))).underlined(1, 10, "хвост"),
            Matchers.equalTo(String.format("[1:10] error: 'хвост'%nпривет%n"))
        );
    }

    @Test
    void rejectsNullSpanAtValidNumber() {
        final List<Span> source = new ArrayList<>(2);
        source.add(new Span("альфа", 1));
        source.add(null);
        Assertions.assertThrows(
            NullPointerException.class,
            () -> new Rows(source).underlined(2, 0, "дыра"),
            "a null span at a valid number is not refused with an exception"
        );
    }
}
