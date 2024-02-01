/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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

import java.nio.file.Path;
import java.util.Map;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.yaml.snakeyaml.Yaml;

/**
 * Test cases for {@link PhiMojo}.
 * @since 0.34.0
 */
class PhiMojoTest {
    @Test
    void createsFiles(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            String.format(
                "There' should be file with .%s extension after translation to phi, but there isn't",
                PhiMojo.EXT
            ),
            new FakeMaven(temp)
                .withProgram(
                    "# This is the default 64+ symbols comment in front of named abstract object.",
                    "[] > cart",
                    "  memory 0 > total",
                    "  [i] > add",
                    "    total.write > @",
                    "      total.plus i"
                )
                .execute(new FakeMaven.Phi())
                .result(),
            Matchers.hasKey(String.format("target/phi/foo/x/main.%s", PhiMojo.EXT))
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/phi", glob = "**.yaml")
    void checksPhiPacks(final String pack, @TempDir final Path temp) throws Exception {
        final Map<String, Object> map = new Yaml().load(pack);
        if (map.get("skip") != null) {
            Assumptions.abort(
                String.format("%s is not ready", pack)
            );
        }
        MatcherAssert.assertThat(
            String.format(
                "Result phi expression should be equal to %s, but it doesn't",
                map.get("phi").toString()
            ),
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(map.get("eo").toString())
                    .execute(new FakeMaven.Phi())
                    .result()
                    .get("target/phi/foo/x/main.phi")
            ).asString(),
            Matchers.equalTo(map.get("phi").toString())
        );
    }
}
