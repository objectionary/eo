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

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Map;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.maven.rust.Names;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.XtoryMatcher;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link BinarizeParse}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.JUnit5TestShouldBePackagePrivate")
@Execution(ExecutionMode.CONCURRENT)
@ExtendWith(MktmpResolver.class)
public final class BinarizeParseTest {

    /**
     * Empty message for JUnit Assertions.
     *
     * @todo #2297:60m Replace all appearances of {@link BinarizeParseTest#TO_ADD_MESSAGE} field in
     *  eo-maven-plugin with meaningful assert messages. Don't forget to remove
     *  {@link BinarizeParseTest#TO_ADD_MESSAGE} field and remove public modifier from this class if
     *  no longer need.
     */
    public static final String TO_ADD_MESSAGE = "TO ADD ASSERTION MESSAGE";

    @Test
    void parsesSimpleEoProgram(@Mktmp final Path temp) throws Exception {
        final Map<String, Path> res = execParse(
            temp,
            BinarizeMojoTest.SRC.resolve("simple-rust.eo")
        );
        final String function = String.format(
            "%s0_QQ_custom_creates_object_r_0",
            Names.PREFIX
        );
        final String rust = String.format(
            "target/Lib/%s/src/foo.rs",
            function
        );
        MatcherAssert.assertThat(
            "The program exists",
            res, Matchers.hasKey(rust)
        );
        MatcherAssert.assertThat(
            "Correct content of Rust file",
            new TextOf(res.get(rust)).asString(),
            Matchers.stringContainsInOrder(
                "use rand::Rng;",
                "pub fn foo(_portal: &Portal) -> Option<EO> {",
                "  let mut rng = rand::thread_rng();",
                "  print!(\"Hello world\");",
                "  let i = rng.gen::<f64>();",
                "  Some(EONumber(i))",
                "}"
            )
        );
        MatcherAssert.assertThat(
            "Correct content of Java file",
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
    void binarizesTwiceRustProgram(@Mktmp final Path temp) throws Exception {
        final Map<String, Path> res = execParse(
            temp,
            BinarizeMojoTest.SRC.resolve("twice-rust.eo")
        );
        final String one = String.format(
            "target/Lib/%s0_QQ_custom_hello_world_1_r_0/src/foo.rs",
            Names.PREFIX
        );
        final String two = String.format(
            "target/Lib/%s1_QQ_custom_hello_world_2_r_0/src/foo.rs",
            Names.PREFIX
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            res, Matchers.hasKey(one)
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            res, Matchers.hasKey(two)
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            new TextOf(res.get(one)).asString(),
            Matchers.stringContainsInOrder(
                "use eo::eo_enum::EO;",
                "use eo::eo_enum::EO::{EONumber};",
                "pub fn foo(_portal: &Portal) -> Option<EO> {",
                "println!(\"{}\", x);"
            )
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            new TextOf(res.get(two)).asString(),
            Matchers.stringContainsInOrder(
                "use eo::eo_enum::EO;",
                "use eo::eo_enum::EO::{EONumber};",
                "pub fn foo(_portal: &Portal) -> Option<EO> {",
                "print!(\"Hello 大 2\");"
            )
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/binarize/add_rust/", glob = "**.yaml")
    void createsDependenciesSection(final String yaml) {
        MatcherAssert.assertThat(
            "passes with no exceptions",
            new XtSticky(new XtYaml(yaml)),
            new XtoryMatcher()
        );
    }

    @Test
    void createsCorrectRustProject(@Mktmp final Path temp) throws Exception {
        final Map<String, Path> res = execParse(
            temp,
            BinarizeMojoTest.SRC.resolve("simple-rust.eo"),
            BinarizeMojoTest.SRC.resolve("twice-rust.eo")
        );
        final String dir = String.format(
            "target/Lib/%s1_QQ_custom_hello_world_2_r_0/",
            Names.PREFIX
        );
        final String cargo = dir.concat("Cargo.toml");
        final String lib = dir.concat("src/lib.rs");
        final String module = String.format(
            "%ssrc/foo.rs",
            dir
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            res, Matchers.hasKey(cargo)
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            res, Matchers.hasKey(lib)
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            res, Matchers.hasKey(module)
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
            new TextOf(res.get(cargo)).asString(),
            Matchers.stringContainsInOrder(
                "[lib]",
                "crate-type = [\"cdylib\"]",
                "[dependencies]",
                "jni = \"0.21.1\""
            )
        );
        MatcherAssert.assertThat(
            BinarizeParseTest.TO_ADD_MESSAGE,
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
    private static Map<String, Path> execParse(final Path temp, final Path... programs)
        throws IOException {
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
            maven.execute(new FakeMaven.Lint());
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
