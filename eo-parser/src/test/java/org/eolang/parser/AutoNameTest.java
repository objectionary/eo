/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import java.util.Locale;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.Isolated;

/**
 * Tests {@link AutoName}.
 *
 * <p>Three of these tests change the process-wide default formatting locale,
 * which every other test of the module would see while they run. The whole
 * class is therefore isolated: nothing else runs beside it.</p>
 *
 * @since 0.58.1
 */
@Isolated
final class AutoNameTest {

    @Test
    void generatesAutoNameForLineAndPos() {
        MatcherAssert.assertThat(
            "Auto name does not match with expected",
            new AutoName(42, 13).asString(),
            Matchers.equalTo("a🌵42-13")
        );
    }

    @Test
    void generatesAsciiDigitsRegardlessOfDefaultLocale() {
        final Locale saved = Locale.getDefault(Locale.Category.FORMAT);
        Locale.setDefault(Locale.Category.FORMAT, new Locale("fa", "IR"));
        try {
            MatcherAssert.assertThat(
                "Auto name is not spelled with ascii digits under fa-IR default locale",
                new AutoName(42, 13).asString(),
                Matchers.equalTo("a🌵42-13")
            );
        } finally {
            Locale.setDefault(Locale.Category.FORMAT, saved);
        }
    }

    @Test
    void generatesAsciiDigitsForZeroesUnderArabicLocale() {
        final Locale saved = Locale.getDefault(Locale.Category.FORMAT);
        Locale.setDefault(Locale.Category.FORMAT, new Locale("ar", "EG"));
        try {
            MatcherAssert.assertThat(
                "Auto name is not spelled with ascii digits under ar-EG default locale",
                new AutoName(0, 0).asString(),
                Matchers.equalTo("a🌵0-0")
            );
        } finally {
            Locale.setDefault(Locale.Category.FORMAT, saved);
        }
    }

    @Test
    void generatesAsciiDigitsForLargeValuesUnderNepaliLocale() {
        final Locale saved = Locale.getDefault(Locale.Category.FORMAT);
        Locale.setDefault(Locale.Category.FORMAT, new Locale("ne", "NP"));
        try {
            MatcherAssert.assertThat(
                "Auto name is not spelled with ascii digits under ne-NP default locale",
                new AutoName(123_456, 7_890).asString(),
                Matchers.equalTo("a🌵123456-7890")
            );
        } finally {
            Locale.setDefault(Locale.Category.FORMAT, saved);
        }
    }
}
