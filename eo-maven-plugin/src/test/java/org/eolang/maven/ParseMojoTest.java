/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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

import java.nio.file.Files;
import java.nio.file.Path;
import org.eolang.parser.ParsingException;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link ParseMojo}.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class ParseMojoTest {

    @Test
    public void testSimpleParsing(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("src");
        final Path target = temp.resolve("target");
        new Save(
            "+package f\n\n[args] > main\n  (stdout \"Hello!\").print\n",
            src.resolve("foo/main.eo")
        ).save();
        new Moja<>(ParseMojo.class)
            .with("sourcesDir", src.toFile())
            .with("targetDir", target.toFile())
            .with("protocolsDir", temp.resolve("protocols").toFile())
            .execute();
        MatcherAssert.assertThat(
            Files.exists(
                target.resolve(
                    String.format("%s/foo/main.eo.xml", ParseMojo.DIR)
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    public void testCrashOnInvalidSyntax(@TempDir final Path temp)
        throws Exception {
        final Path src = temp.resolve("src");
        final Path target = temp.resolve("target");
        new Save("something is wrong here", src.resolve("f.eo")).save();
        Assertions.assertThrows(
            ParsingException.class,
            () -> new Moja<>(ParseMojo.class)
                .with("sourcesDir", src.toFile())
                .with("targetDir", target.toFile())
                .with("protocolsDir", temp.resolve("proto").toFile())
                .execute()
        );
    }

}
