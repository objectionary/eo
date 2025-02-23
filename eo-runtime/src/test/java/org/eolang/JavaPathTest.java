/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test case for {@link JavaPath}.
 *
 * @since 0.29
 */
class JavaPathTest {

    @ParameterizedTest
    @CsvSource({
        "Φ.org.eolang, EOorg.EOeolang",
        "Φ.obj, EOobj",
        "Φ.obj.sub, EOobj.EOsub",
        "Φ.obj.sub$attr, EOobj.EOsub$EOattr",
        "Φ.obj.sub-dashed$attr, EOobj.EOsub_dashed$EOattr",
        "Φ.obj.sub-dashed$attr-dashed, EOobj.EOsub_dashed$EOattr_dashed"
    })
    void convertsToString(final String name, final String expected) {
        MatcherAssert.assertThat(
            AtCompositeTest.TO_ADD_MESSAGE,
            new JavaPath(name).toString(),
            Matchers.equalTo(expected)
        );
    }
}
