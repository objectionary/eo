/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Test case for {@link Kept}.
 * @since 0.69.0
 */
final class KeptTest {

    @Test
    void writesBackAFormItKnowsNothingAbout() {
        MatcherAssert.assertThat(
            "a form nobody here understands must come back as it was, but it didnt",
            new XMLDocument(
                new Xembler(
                    new Directives().add("type").append(
                        new Kept(
                            new Xnav(
                                new XMLDocument(
                                    "<links><type id='a'><union k='1'><data/></union></type></links>"
                                ).inner()
                            ).element("links").element("type")
                        ).directives()
                    )
                ).domQuietly()
            ),
            XhtmlMatchers.hasXPath("/type/union[@k='1']/data")
        );
    }
}
