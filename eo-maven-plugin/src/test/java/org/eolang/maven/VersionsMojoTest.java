/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.nio.file.Path;
import org.eolang.maven.hash.CommitHashesMap;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test for {@link VersionsMojo}.
 *
 * @since 0.29.6
 */
final class VersionsMojoTest {
    @Test
    void replacesVersionsOk(@TempDir final Path tmp) throws Exception {
        new FakeMaven(tmp)
            .with("withVersions", true)
            .with("commitHashes", new CommitHashesMap.Fake())
            .withProgram(
                "+alias org.eolang.io.stdout\n",
                "[] > main",
                "  seq > @",
                "    stdout|0.23.17",
                "      QQ.txt.sprintf|0.25.0",
                "        \"Hello world\"",
                "    nop"
            ).execute(new FakeMaven.Versions());
        final XML xml = new XMLDocument(
            tmp.resolve(
                String.format("target/%s/foo/x/main.xmir", ParseMojo.DIR)
            )
        );
        final String format = "//o[@ver and (@ver='%s' or @ver='%s')]/@ver";
        final int size = 2;
        MatcherAssert.assertThat(
            String.format(
                "XMIR after replacing the versions should have contained %d elements <o> with hashes in \"ver\" attribute, but it didn't",
                size
            ),
            xml.xpath(String.format(format, "15c85d7", "0aa6875")),
            Matchers.hasSize(size)
        );
        MatcherAssert.assertThat(
            "XMIR after replacing the versions should not have contained elements <o> with tags in \"ver\" attribute, but it did",
            xml.xpath(String.format(format, "0.23.17", "0.25.0")),
            Matchers.empty()
        );
    }
}
