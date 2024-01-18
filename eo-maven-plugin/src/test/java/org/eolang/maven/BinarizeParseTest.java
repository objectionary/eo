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

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.maven.rust.Names;
import org.eolang.xax.XaxStory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link BinarizeParse}.
 *
 * @since 0.1
 */
@Execution(ExecutionMode.CONCURRENT)
final class BinarizeParseTest {

    @Test
    void parsesSimpleEoProgram(@TempDir final Path temp) throws Exception {
        final Map<String, Path> res = execParse(
            temp,
            BinarizeMojoTest.SRC.resolve("simple-rust.eo")
        );
        final String function = String.format(
            "%s0",
            Names.PREFIX
        );
        final String rust = String.format(
            "target/Lib/%s/src/foo.rs",
            function
        );
        MatcherAssert.assertThat(
            res, Matchers.hasKey(rust)
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(rust)).asString(),
            Matchers.stringContainsInOrder(
                "use rand::Rng;",
                "pub fn foo(_portal: &Portal) -> Option<EO> {",
                "  let mut rng = rand::thread_rng();",
                "  print!(\"Hello world\");",
                "  let i = rng.gen::<i64>();",
                "  Some(EOInt(i))",
                "}"
            )
        );
        MatcherAssert.assertThat(
            new TextOf(
                res.get(
                    String.format(
                        "target/generated/EOrust/natives/%s.java",
                        function
                    )
                )
            ).asString(),
            Matchers.containsString("public static native byte[]")
        );
    }

    @Test
    void binarizesTwiceRustProgram(@TempDir final Path temp) throws Exception {
        final Map<String, Path> res = execParse(
            temp,
            BinarizeMojoTest.SRC.resolve("twice-rust.eo")
        );
        final String one = String.format(
            "target/Lib/%s0/src/foo.rs",
            Names.PREFIX
        );
        final String two = String.format(
            "target/Lib/%s1/src/foo.rs",
            Names.PREFIX
        );
        MatcherAssert.assertThat(
            res, Matchers.hasKey(one)
        );
        MatcherAssert.assertThat(
            res, Matchers.hasKey(two)
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(one)).asString(),
            Matchers.stringContainsInOrder(
                "use eo::eo_enum::EO;",
                "use eo::eo_enum::EO::{EOInt};",
                "pub fn foo(_portal: &Portal) -> Option<EO> {",
                "println!(\"{}\", x);"
            )
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(two)).asString(),
            Matchers.stringContainsInOrder(
                "use eo::eo_enum::EO;",
                "use eo::eo_enum::EO::{EOInt};",
                "pub fn foo(_portal: &Portal) -> Option<EO> {",
                "print!(\"Hello 大 2\");"
            )
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/binarize/add_rust/", glob = "**.yaml")
    void createsDependenciesSection(final String yaml) {
        MatcherAssert.assertThat(
            new XaxStory(yaml),
            Matchers.is(true)
        );
    }

    @Test
    void createsCorrectRustProject(@TempDir final Path temp) throws Exception {
        final Map<String, Path> res = execParse(
            temp,
            BinarizeMojoTest.SRC.resolve("simple-rust.eo"),
            BinarizeMojoTest.SRC.resolve("twice-rust.eo")
        );
        final String dir = String.format(
            "target/Lib/%s1/",
            Names.PREFIX
        );
        final String cargo = dir.concat("Cargo.toml");
        final String lib = dir.concat("src/lib.rs");
        final String module = String.format(
            "%ssrc/foo.rs",
            dir
        );
        MatcherAssert.assertThat(
            res, Matchers.hasKey(cargo)
        );
        MatcherAssert.assertThat(
            res, Matchers.hasKey(lib)
        );
        MatcherAssert.assertThat(
            res, Matchers.hasKey(module)
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(cargo)).asString(),
            Matchers.stringContainsInOrder(
                "[lib]",
                "crate-type = [\"cdylib\"]",
                "[dependencies]",
                "jni = \"0.21.1\""
            )
        );
        MatcherAssert.assertThat(
            new TextOf(res.get(cargo)).asString(),
            Matchers.stringContainsInOrder(
                "[dependencies]",
                "rand = \"0.5.5\""
            )
        );
    }

    /**
     * Executes all mojos before {@link BinarizeMojo} and {@link BinarizeParse}.
     * @param temp Temp dir.
     * @param programs Paths of eo sources.
     * @return The resulting map with relatives paths as keys and absolute paths as values.
     * @throws IOException
     */
    private static
        Map<String, Path> execParse(final Path temp, final Path... programs) throws IOException {
        final FakeMaven maven;
        final File generated = temp.resolve("target/generated").toFile();
        final File portal = new File("../eo-runtime/src/main/rust/eo");
        final File target = temp.resolve("target").toFile();
        synchronized (BinarizeParseTest.class) {
            maven = new FakeMaven(temp)
                .with("generatedDir", generated)
                .with("eoPortalDir", portal)
                .with("targetDir", target);
            for (final Path program : programs) {
                maven.withProgram(program);
            }
            maven.execute(new FakeMaven.Verify());
        }
        new BinarizeParse(
            generated,
            portal,
            new Names(generated.toPath().resolve("names")),
            target
        ).exec(maven.foreignTojos().withOptimized());
        return maven.result();
    }
}
