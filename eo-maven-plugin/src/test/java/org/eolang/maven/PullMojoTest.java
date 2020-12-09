/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.LengthOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link PullMojo}.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class PullMojoTest {

    @Test
    @Disabled
    public void testSimpleOptimize() throws Exception {
        final Path temp = Files.createTempDirectory("eo");
        final Path src = temp.resolve("src");
        new LengthOf(
            new TeeInput(
                new InputOf(
                    String.join(
                        "\n",
                        "+alias stdout org.eolang.io.stdout",
                        "",
                        "[args] > main\n  (stdout \"Hello!\").print\n"
                    )
                ),
                new OutputTo(src.resolve("main.eo"))
            )
        ).value();
        final Path target = temp.resolve("target");
        new Mojo<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("sourcesDir", src.toFile())
            .execute();
        new Mojo<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .execute();
        new Mojo<>(PullMojo.class)
            .with("targetDir", target.toFile())
            .execute();
        MatcherAssert.assertThat(
            Files.exists(target.resolve("eo/pull/org/stdout/io/stdout.eo.xml")),
            Matchers.is(true)
        );
    }

}
