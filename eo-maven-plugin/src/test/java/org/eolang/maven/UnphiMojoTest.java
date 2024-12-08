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

import com.jcabi.log.Logger;
import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.cactoos.io.InputOf;
import org.cactoos.list.ListOf;
import org.cactoos.set.SetOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.maven.util.HmBase;
import org.eolang.parser.EoSyntax;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test cases for {@link UnphiMojo}.
 * @since 0.34.0
 */
@ExtendWith(MktmpResolver.class)
final class UnphiMojoTest {

    @Test
    @ExtendWith(WeAreOnline.class)
    void convertsPhiToXmir(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("target/eo/phi/foo.phi").write(
                    "{ ⟦ a ↦ Φ.org.eolang.bytes ( α0 ↦ ⟦ Δ ⤍ 00- ⟧ ) ⟧}".getBytes(
                        StandardCharsets.UTF_8
                    )
                );
                f.build()
                    .plugins()
                    .appendItself()
                    .execution()
                    .phase("initialize")
                    .goals("phi-to-xmir");
                f.exec("initialize");
            }
        );
        MatcherAssert.assertThat(
            "the .xmir file is generated",
            XhtmlMatchers.xhtml(
                new String(
                    Files.readAllBytes(
                        temp.resolve("target/eo/1-parse/foo.xmir")
                    )
                )
            ),
            XhtmlMatchers.hasXPaths("/program/objects/o[text()='00-']")
        );
    }

    @Test
    void failsOnBrokenPhiSyntax(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("target/eo/phi/foo.phi").write(
                    "{ ⟦ a ↦ Φ.org.eolang.bytes ( Δ ⤍ 00- ) ⟧}".getBytes(StandardCharsets.UTF_8)
                );
                f.build()
                    .plugins()
                    .appendItself()
                    .execution()
                    .phase("initialize")
                    .goals("phi-to-xmir");
                MatcherAssert.assertThat(
                    "fails because of broken phi syntax",
                    f.execQuiet("initialize"),
                    Matchers.not(Matchers.equalTo(0))
                );
            }
        );
    }

    @Test
    void createsFile(@Mktmp final Path temp) throws Exception {
        new HmBase(temp).save(
            "{⟦std ↦ Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x⟧}",
            Paths.get("target/phi/std.phi")
        );
        MatcherAssert.assertThat(
            String.format(
                "There should be file with .%s extension after parsing phi to XMIR, but there isn't",
                AssembleMojo.XMIR
            ),
            new FakeMaven(temp)
                .execute(UnphiMojo.class)
                .result(),
            Matchers.hasKey(String.format("target/%s/std.xmir", ParseMojo.DIR))
        );
    }

    @Test
    void failsIfParsedWithErrors(@Mktmp final Path temp) throws IOException {
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

    @Test
    void addsMetas(@Mktmp final Path temp) throws IOException {
        new HmBase(temp).save(
            "{⟦std ↦ Φ.org.eolang.io.stdout⟧}",
            Paths.get("target/phi/std.phi")
        );
        MatcherAssert.assertThat(
            "Unphied XMIR must contain metas, added via \"unphiMetas\" parameter",
            new XMLDocument(
                new FakeMaven(temp)
                    .with(
                        "unphiMetas",
                        new SetOf<>("+tests", "+home https://github.com/objectionary/eo")
                    )
                    .execute(UnphiMojo.class)
                    .result()
                    .get(String.format("target/%s/std.xmir", ParseMojo.DIR))
            ),
            XhtmlMatchers.hasXPaths(
                "/program/metas/meta[head/text()='tests' and not(tail/text())]",
                "/program/metas/meta[head/text()='home' and tail/text()='https://github.com/objectionary/eo']"
            )
        );
    }

    @Test
    void failsIfPackageMetaIsAdded(@Mktmp final Path temp) throws IOException {
        new HmBase(temp).save(
            "{⟦std ↦ Φ.org.eolang.io.stdout⟧}",
            Paths.get("target/phi/std.phi")
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .with("unphiMetas", new SetOf<>("+package org.eolang"))
                .execute(UnphiMojo.class),
            "UnphiMojo execution should fail if \"+package\" meta is added"
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/unphi", glob = "**.yaml")
    @SuppressWarnings("unchecked")
    void checksUnphiPacks(final String pack, @Mktmp final Path temp) throws Exception {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        final String phi = xtory.map().get("phi").toString();
        new HmBase(temp).save(phi, Paths.get("target/phi/main.phi"));
        final List<String> failures = new ListOf<>();
        new FakeMaven(temp).execute(UnphiMojo.class);
        final XML doc = new XMLDocument(
            new TextOf(
                temp.resolve(
                    Paths.get(String.format("target/%s/main.xmir", ParseMojo.DIR))
                )
            ).asString()
        );
        Logger.debug(this, "Parsed phi:\n%s", doc);
        for (final String xpath : (Iterable<String>) xtory.map().get("asserts")) {
            final List<XML> nodes = doc.nodes(xpath);
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
    void convertsToXmirAndBack(final String pack, @Mktmp final Path temp) throws Exception {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        final String phi = xtory.map().get("phi").toString();
        final String main = "target/phi/main.phi";
        final Path path = Paths.get(main);
        new HmBase(temp).save(phi, path);
        final long saved = temp.resolve(path).toFile().lastModified();
        final FakeMaven maven = new FakeMaven(temp).execute(UnphiMojo.class);
        final Path xmir = temp.resolve(String.format("target/%s/main.xmir", ParseMojo.DIR));
        maven.foreignTojos().add("name").withXmir(xmir);
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
            new TextOf(result).asString(),
            Matchers.equalTo(phi)
        );
    }

    @ParameterizedTest
    @CsvSource({"true", "false"})
    void convertsValidXmirAndParsableEO(final boolean reversed, @Mktmp final Path temp)
        throws Exception {
        final Map<String, Path> map = new FakeMaven(temp)
            .withProgram(
                "# No comments.",
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
            XhtmlMatchers.hasXPath("/program[not(errors)]")
        );
    }
}
