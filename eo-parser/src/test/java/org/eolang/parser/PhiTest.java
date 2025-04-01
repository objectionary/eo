/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.matchers.XhtmlMatchers;
import java.io.IOException;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link Phi}.
 * @since 0.56
 */
final class PhiTest {

    @Test
    void appliesUnphiTransformationOnNumbers() throws IOException {
        MatcherAssert.assertThat(
            "We should see preserve all bytes after PHI/UNPHI transformations",
            new Phi(new Xmir(new EoSyntax("1 > phi").parsed()).toPhi()).unphi(),
            XhtmlMatchers.hasXPath(
                "//o[@base='Q.org.eolang.number']/o[@base='Q.org.eolang.bytes']/o[text()='3F-F0-00-00-00-00-00-00']"
            )
        );
    }
}
