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

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XaxStory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link BinarizeMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class BinarizeMojoTest {

    @Test
    void binarizesSimpleEoProgram(@TempDir final Path temp) throws Exception {
        final Path src = Paths.get("src/test/resources/org/eolang/maven/simple-rust.eo");
        final Map<String, Path> res = new FakeMaven(temp)
            .withProgram(src)
            .execute(new FakeMaven.Binarize())
            .result();
        final String rust = "target/binarize/codes/Φ_org_eolang_custom_creates_object_r.rs";
        MatcherAssert.assertThat(
            res, Matchers.hasKey(rust)
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(rust)).asString(),
            Matchers.stringContainsInOrder(
                "use reo::universe::Universe;",
                    "use reo::data::Data;",
                    "use rand::rand;",
                    "pub fn foo(uni: &mut Universe, v: u32) -> Result<u32> {",
                    "  print!(\"Hello world\");",
                    "  let i = copy!(find!(\"org.eolang.int\"));",
                    "  uni.data(i, Data::from_int(random::<i64>()))?;",
                    "  Ok(i)",
                    "}"
            )
        );
    }

    @Test
    void binarizesTwiceRustProgram(@TempDir final Path temp) throws Exception {
        final Path src = Paths.get("src/test/resources/org/eolang/maven/twice-rust.eo");
        final Map<String, Path> res = new FakeMaven(temp)
            .withProgram(src)
            .execute(new FakeMaven.Binarize())
            .result();
        final String one = "target/binarize/codes/Φ_org_eolang_custom_hello_world_1_r.rs";
        final String two = "target/binarize/codes/Φ_org_eolang_custom_hello_world_2_r.rs";
        MatcherAssert.assertThat(
            res, Matchers.hasKey(one)
        );
        MatcherAssert.assertThat(
            res, Matchers.hasKey(two)
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(one)).asString(),
            Matchers.stringContainsInOrder(
                "use reo::universe::Universe;",
                "pub fn foo(uni: &mut Universe, v: u32) {",
                "print!(\"Hello world 1\");"
            )
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(two)).asString(),
            Matchers.stringContainsInOrder(
                "use reo::universe::Universe;",
                "pub fn foo(uni: &mut Universe, v: u32) {",
                "print!(\"Hello world 2\");"
                )
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/add_rust/", glob = "**.yaml")
    void createsDependenciesSection(final String yaml) {
        MatcherAssert.assertThat(
            new XaxStory(yaml),
            Matchers.is(true)
        );
    }
}
