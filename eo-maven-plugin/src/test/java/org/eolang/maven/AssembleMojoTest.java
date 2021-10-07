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
import org.cactoos.Func;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link AssembleMojo}.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class AssembleMojoTest {

    @Test
    public void testSimplePull(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("src");
        new Save(
            String.join(
                "\n",
                "+alias stdout org.eolang.io.stdout",
                "",
                "[x] > main\n  (stdout \"Hello!\" x).print\n"
            ),
            src.resolve("main.eo")
        ).save();
        final Path target = temp.resolve("target");
        new Moja<>(RegisterMojo.class)
            .with("foreign", temp.resolve("eo-foreign.csv").toFile())
            .with("sourcesDir", src.toFile())
            .execute();
        new Moja<>(AssembleMojo.class)
            .with("outputDir", temp.resolve("out").toFile())
            .with("targetDir", target.toFile())
            .with("foreign", temp.resolve("eo-foreign.csv").toFile())
            .with("resolvedList", temp.resolve("list").toFile())
            .with("skipZeroVersions", true)
            .with("overWrite", true)
            .with(
                "objectionary",
                (Func<String, Input>) input -> new InputOf(
                    "[] > sprintf\n"
                )
            )
            .execute();
        MatcherAssert.assertThat(
            Files.exists(
                target.resolve(
                    String.format(
                        "%s/org/eolang/io/stdout.eo.xml",
                        ParseMojo.DIR
                    )
                )
            ),
            Matchers.is(true)
        );
    }

}
