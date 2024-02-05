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

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.cactoos.io.InputOf;
import org.cactoos.list.ListOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.maven.util.HmBase;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.yaml.snakeyaml.Yaml;

/**
 * Test cases for {@link UnphiMojo}.
 * @since 0.34.0
 */
class UnphiMojoTest {
    @Test
    void createsFile(@TempDir final Path temp) throws Exception {
        new HmBase(temp).save(
            "{std ↦ Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x}",
            Paths.get("target/phi/std.phi")
        );
        MatcherAssert.assertThat(
            String.format(
                "There should be file with .%s extension after parsing phi to XMIR, but there isn't",
                TranspileMojo.EXT
            ),
            new FakeMaven(temp)
                .execute(UnphiMojo.class)
                .result(),
            Matchers.hasKey(String.format("target/%s/std.xmir", ParseMojo.DIR))
        );
    }

    @Test
    void failsIfParsedWithErrors(@TempDir final Path temp) throws IOException {
        new HmBase(temp).save(
            "std ↦ Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x",
            Paths.get("target/phi/std.phi")
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .execute(UnphiMojo.class),
            "UnphiMojo execution should fail because of parsing errors"
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/unphi", glob = "**.yaml")
    void checksUnphiPacks(final String pack, @TempDir final Path temp) throws Exception {
        final Map<String, Object> map = new Yaml().load(pack);
        final String phi = map.get("phi").toString();
        new HmBase(temp).save(phi, Paths.get("target/phi/main.phi"));
        final List<String> failures = new ListOf<>();
        new FakeMaven(temp).execute(UnphiMojo.class);
        for (final String xpath : (Iterable<String>) map.get("tests")) {
            final List<XML> nodes = new XMLDocument(
                new TextOf(
                    temp.resolve(
                        Paths.get(String.format("target/%s/main.xmir", ParseMojo.DIR))
                    )
                ).asString()
            ).nodes(xpath);
            if (nodes.isEmpty()) {
                failures.add(xpath);
            }
        }
        MatcherAssert.assertThat(
            String.format(
                "Failed to parse phi expression: %s; failed tests: %s",
                phi, Arrays.toString(failures.toArray())
            ),
            failures,
            Matchers.empty()
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/phi", glob = "**.yaml")
    void convertsToXmirAndBack(final String pack, @TempDir final Path temp) throws Exception {
        final Map<String, Object> map = new Yaml().load(pack);
        if (map.get("skip") != null) {
            Assumptions.abort(
                String.format("%s is not ready", pack)
            );
        }
        final String phi = map.get("phi").toString();
        final String main = "target/phi/main.phi";
        final Path path = Paths.get(main);
        new HmBase(temp).save(phi, path);
        final long saved = temp.resolve(path).toFile().lastModified();
        final FakeMaven maven = new FakeMaven(temp).execute(UnphiMojo.class);
        maven.foreignTojos().add("name")
            .withXmir(temp.resolve(String.format("target/%s/main.xmir", ParseMojo.DIR)));
        final Path result = maven
            .execute(OptimizeMojo.class)
            .execute(PhiMojo.class)
            .result()
            .get(main);
        MatcherAssert.assertThat(
            String.format("%s should have been rewritten after optimization, but it wasn't", main),
            result.toFile().lastModified(),
            Matchers.greaterThan(saved)
        );
        MatcherAssert.assertThat(
            "Origin phi should equal to phi got from \"unphied\" xmir, but it isn't",
            phi,
            Matchers.equalTo(
                new TextOf(result).asString()
            )
        );
    }

    @ParameterizedTest
    @CsvSource({"true", "false"})
    void convertsValidXmirAndParsableEO(
        final boolean reversed,
        @TempDir final Path temp
    ) throws Exception {
        final Map<String, Path> map = new FakeMaven(temp)
            .withProgram(
                "# This is the default 64+ symbols comment in front of named abstract object.",
                "[args] > app",
                "  QQ.io.stdout > @",
                "    \"Hello, world!\""
            )
            .with("printSourcesDir", temp.resolve("target/1-parse").toFile())
            .with("printOutputDir", temp.resolve("target/generated-sources").toFile())
            .with("printReversed", reversed)
            .execute(ParseMojo.class)
            .execute(OptimizeMojo.class)
            .execute(PhiMojo.class)
            .execute(UnphiMojo.class)
            .execute(PrintMojo.class)
            .result();
        MatcherAssert.assertThat(
            "Result EO code should be parsable",
            new EoSyntax(
                "test",
                new InputOf(
                    new TextOf(
                        temp.resolve(
                            map.get("target/generated-sources/foo/x/main.eo")
                        )
                    )
                )
            ).parsed(),
            XhtmlMatchers.hasXPath("//errors[count(error)=0]")
        );
    }
}
