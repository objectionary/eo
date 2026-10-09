/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.lowering;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.xml.transform.TransformerException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests of the class {@link Hrefs}.
 *
 * @since 0.64.0
 */
@ExtendWith(MktmpResolver.class)
final class HrefsTest {

    @Test
    void readsAStylesheetOfTheModuleFromTheClasspath() throws TransformerException {
        MatcherAssert.assertThat(
            "the stylesheet the path names must be read from the classpath, but it wasnt",
            new XMLDocument(
                new Hrefs().resolve("/org/eolang/lowering/_returns.xsl", "")
            ).xpath("/*/@id"),
            Matchers.contains("_returns")
        );
    }

    @Test
    void readsATableFromTheDisk(@Mktmp final Path temp)
        throws IOException, TransformerException {
        MatcherAssert.assertThat(
            "the table the URI names must be read from the disk, but it wasnt",
            new XMLDocument(
                new Hrefs().resolve(
                    Files.write(
                        temp.resolve("links.xml"),
                        "<links><type id='Φ.q7w.φ'/></links>".getBytes(StandardCharsets.UTF_8)
                    ).toUri().toString(),
                    ""
                )
            ).xpath("/links/type/@id"),
            Matchers.contains("Φ.q7w.φ")
        );
    }
}
