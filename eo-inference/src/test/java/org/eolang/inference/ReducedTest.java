/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Reduced}.
 *
 * @since 0.73.0
 */
@ExtendWith(MktmpResolver.class)
final class ReducedTest {

    @Test
    void leavesPartialApplicationUnreduced(@Mktmp final Path temp) throws IOException {
        final Path tables = Files.createDirectories(temp.resolve("tables"));
        Files.writeString(
            tables.resolve("provides.xml"),
            String.join(
                "",
                "<provides>",
                "<type id='Φ.oak'>",
                "<attr name='seed' type='Φ.oak.seed' void='true'/>",
                "</type>",
                "<type id='Φ.alias'>",
                "<attr name='φ' type='Φ.alias.φ'/>",
                "</type>",
                "</provides>"
            )
        );
        Files.writeString(
            tables.resolve("links.xml"),
            String.join(
                "",
                "<links><type id='Φ.alias.φ'><ref loc='Φ.oak'>",
                "<bind void='Φ.oak.seed'><ref loc='Φ.elm'/></bind>",
                "</ref></type></links>"
            )
        );
        new Reduced((xmirs, path) -> { }).follow(temp.resolve("xmirs"), tables);
        MatcherAssert.assertThat(
            "a body that fills a void of its base type must keep the name of its type",
            new XMLDocument(Files.readString(tables.resolve("provides.xml"))),
            XhtmlMatchers.hasXPath("/provides/type[@id='Φ.alias' and not(@reduced)]")
        );
    }
}
