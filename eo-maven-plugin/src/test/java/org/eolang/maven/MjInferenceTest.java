/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjInference}.
 *
 * @since 0.67.0
 */
@ExtendWith(MktmpResolver.class)
final class MjInferenceTest {

    @Test
    void writesTableOfWholeProgram(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the table must say what the innermost formation provides, but it doesnt",
            new XMLDocument(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "[] > app",
                        "  [] > t",
                        "    [] > next",
                        ""
                    )
                )
                .execute(MjParse.class)
                .execute(MjInference.class)
                .targetPath()
                .resolve("6-inference")
                .resolve("provides.xml")
            ),
            XhtmlMatchers.hasXPath("/provides/type[@id='Φ.app.t']/attr[@name='next']")
        );
    }

    @Test
    void keepsPreparedXmirOfProgram(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "the prepared XMIR must have one object per dispatch step, but it doesnt",
            new XMLDocument(
                new FakeMaven(temp).withProgram(
                    String.join(
                        System.lineSeparator(),
                        "[x] > box",
                        "  x.lid.hinge > @",
                        ""
                    )
                )
                .execute(MjParse.class)
                .execute(MjInference.class)
                .targetPath()
                .resolve("6-pre-inference")
                .resolve("foo")
                .resolve("x")
                .resolve("main.xmir")
            ),
            XhtmlMatchers.hasXPath("//o[@base='.hinge']/o[@base='.lid']/o[@base='ξ.x']")
        );
    }
}
