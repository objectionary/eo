/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.printer;

import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.StClasspath;
import com.yegor256.xsline.Xsline;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test cases for helper namespaces in XSL results.
 * @since 0.60
 */
final class XslNamespacesTest {

    @Test
    void doesNotLeakHelperNamespaces() {
        final String result = new Xsline(
            new StClasspath("/org/eolang/printer/print/to-eo-tree.xsl")
        ).pass(
            new XMLDocument(
                "<object><metas/><o name='main'/></object>"
            )
        ).toString();
        MatcherAssert.assertThat(
            "XSL helper namespaces must not be serialized into printer XML",
            result,
            Matchers.allOf(
                Matchers.not(Matchers.containsString("xmlns:eo=")),
                Matchers.not(Matchers.containsString("xmlns:xs="))
            )
        );
    }
}
