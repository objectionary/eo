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
import org.eolang.tojos.MonoTojos;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link OptimizeMojo}.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
public final class OptimizeMojoTest {

    @Test
    public void testSimpleOptimize(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo/main.eo");
        new Save(
            "+package f\n\n[args] > main\n  (stdout \"Hello!\").print > @\n",
            src
        ).save();
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.csv");
        new MonoTojos(foreign).add("foo.main").set("eo", src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .execute();
        MatcherAssert.assertThat(
            Files.exists(
                target.resolve(
                    String.format("%s/foo/main/00-not-empty-atoms.xml", OptimizeMojo.STEPS)
                )
            ),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            Files.exists(
                target.resolve(
                    String.format("%s/foo/main.eo.xml", OptimizeMojo.DIR)
                )
            ),
            Matchers.is(true)
        );
    }

}
