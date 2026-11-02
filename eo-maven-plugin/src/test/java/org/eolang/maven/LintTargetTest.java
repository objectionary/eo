/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.File;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link LintTarget}.
 *
 * @since 0.64
 */
@ExtendWith(MktmpResolver.class)
final class LintTargetTest {

    @Test
    void namesTheTargetAfterTheObjectTheDocumentDeclares(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "the target must be named after the object of the document, but it wasnt",
            new LintTarget(
                new XMLDocument("<object><o name='bar'/></object>"),
                temp.resolve("bar.xmir"),
                "foo.x.bar"
            ).under(temp).toString(),
            Matchers.endsWith(String.format("bar.%s", MjAssemble.XMIR))
        );
    }

    @Test
    void namesTheTargetAfterTheCatalogWhenTheDocumentDeclaresNoObject(@Mktmp final Path temp) {
        MatcherAssert.assertThat(
            "a document that declares no object must be named by the catalog, not abort the goal",
            new LintTarget(
                new XMLDocument("<object><metas/></object>"),
                temp.resolve("main.xmir"),
                "foo.x.main"
            ).under(temp).toString(),
            Matchers.endsWith(
                String.format("foo%sx%smain.%s", File.separator, File.separator, MjAssemble.XMIR)
            )
        );
    }
}
