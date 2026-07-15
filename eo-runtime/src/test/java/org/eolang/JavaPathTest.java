/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link JavaPath}.
 * @since 0.29
 */
class JavaPathTest {

    @ParameterizedTest
    @CsvSource({
        "Φ.number, org.eolang.EOnumber",
        "Φ.org.eolang, org.eolang.EO_org.EOeolang",
        "Φ.number.power, org.eolang.EO_number.EOpower",
        "Φ.obj, org.eolang.EOobj",
        "Φ.obj.sub, org.eolang.EO_obj.EOsub",
        "Φ.obj.sub$attr, org.eolang.EO_obj.EOsub$EOattr",
        "Φ.obj.sub-dashed$attr, org.eolang.EO_obj.EOsub_dashed$EOattr",
        "Φ.obj.sub-dashed$attr-dashed, org.eolang.EO_obj.EOsub_dashed$EOattr_dashed"
    })
    void convertsToString(final String name, final String expected) {
        MatcherAssert.assertThat(
            String.format("JavaPath should convert '%s' to '%s', but it didn't", name, expected),
            new JavaPath(name).toString(),
            Matchers.equalTo(expected)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "Φ.number, org.eolang.EO_number",
        "Φ.number.power, org.eolang.EO_number.EO_power",
        "Φ.obj.sub, org.eolang.EO_obj.EO_sub"
    })
    void convertsToPackage(final String name, final String expected) {
        MatcherAssert.assertThat(
            String.format("JavaPath should convert '%s' to package '%s', but it didn't", name, expected),
            new JavaPath(name).pkg(),
            Matchers.equalTo(expected)
        );
    }
}
