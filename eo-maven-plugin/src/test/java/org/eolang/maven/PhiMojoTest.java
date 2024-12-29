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
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import com.yegor256.farea.Requisite;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
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
import org.junitpioneer.jupiter.ExpectedToFail;

/**
 * Test cases for {@link PhiMojo}.
 * @since 0.34.0
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.AvoidDuplicateLiterals"})
@ExtendWith(MktmpResolver.class)
@ExtendWith(RandomProgramResolver.class)
final class PhiMojoTest {

    @Test
    void convertsSimpleObjectToPhi(@Mktmp final Path temp, @RandomProgram final String program)
        throws Exception {
        this.executeXmirToPhi(new Farea(temp), program);
        MatcherAssert.assertThat(
            "the .phi file is generated",
            Files.readString(temp.resolve("target/eo/phi/foo.phi")),
            Matchers.containsString("(\"Hello, world!\\n\")")
        );
    }

    @Test
    void convertsSimpleXmirToPhi(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("target/eo/2-optimize/foo.xmir").write(
                    String.join(
                        " ",
                        "<program name='foo'><objects>",
                        "<o name='foo'>",
                        "<o name='bar' base='xxx'>",
                        "<o base='org.eolang.bytes'>01-02-03</o>",
                        "</o></o></objects></program>"
                    ).getBytes()
                );
                f.build()
                    .plugins()
                    .appendItself()
                    .execution()
                    .goals("xmir-to-phi");
                f.exec("compile");
            }
        );
        MatcherAssert.assertThat(
            "the .phi file is generated",
            Files.readString(temp.resolve("target/eo/phi/foo.phi")),
            Matchers.containsString("(⟦ Δ ⤍ 01-02-03 ⟧)")
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    void convertsObjectWithSystemType(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/org/eolang/bytes.eo").write(
                    String.join(
                        "\n",
                        "+package org.eolang",
                        "",
                        "# In this program, we define a 'system' object, similar",
                        "# to how it is defined in org.eolang package, trying to",
                        "# reproduce the error.",
                        "[] > bytes",
                        "  $.eq 01-02-03 > yes",
                        ""
                    ).getBytes()
                );
                f.build().plugins().appendItself();
                f.exec("eo:register", "eo:parse", "eo:optimize");
                f.exec("eo:xmir-to-phi");
            }
        );
        MatcherAssert.assertThat(
            "the .xmir file is generated",
            XhtmlMatchers.xhtml(
                Files.readString(temp.resolve("target/eo/2-optimize/org/eolang/bytes.xmir"))
            ),
            XhtmlMatchers.hasXPaths(
                "/program/objects/o[@name='bytes']",
                "/program/objects/o/o[@base='.eq']",
                "/program/objects/o/o/o[@base='org.eolang.bytes' and text()='01-02-03']"
            )
        );
        MatcherAssert.assertThat(
            "the .phi file is generated",
            Files.readString(temp.resolve("target/eo/phi/org/eolang/bytes.phi")),
            Matchers.allOf(
                Matchers.containsString("yes ↦ ξ.eq("),
                Matchers.containsString("Φ̇.bytes(⟦ Δ ⤍ 01-02-03 ⟧)")
            )
        );
    }

    @Test
    void createsFiles(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            String.format(
                "There' should be file with .%s extension after translation to phi, but there isn't",
                PhiMojo.EXT
            ),
            new FakeMaven(temp)
                .withProgram(
                    "No comments.",
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

    @Test
    void doesNotFailOnError(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withProgram(
                    "No comments.",
                    "[] > without-name",
                    "  true"
                )
                .execute(new FakeMaven.Phi()),
            "PhiMojo should not fail on errors"
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/phi-packs", glob = "**.yaml")
    void checksPhiPacksWithSugar(final String pack, @Mktmp final Path temp) throws Exception {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        MatcherAssert.assertThat(
            "must convert to exactly the expression we need with syntax sugar",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(xtory.map().get("input").toString())
                    .with("phiNoSugar", false)
                    .execute(new FakeMaven.Phi())
                    .result()
                    .get("target/phi/foo/x/main.phi")
            ).asString(),
            Matchers.equalTo(xtory.map().get("with-sugar").toString())
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/phi-packs", glob = "**.yaml")
    void checksPhiPacksNoSugar(final String pack, @Mktmp final Path temp) throws Exception {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        MatcherAssert.assertThat(
            "must convert to exactly the expression we need without syntax sugar",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(xtory.map().get("input").toString())
                    .with("phiNoSugar", true)
                    .execute(new FakeMaven.Phi())
                    .result()
                    .get("target/phi/foo/x/main.phi")
            ).asString(),
            Matchers.equalTo(xtory.map().get("no-sugar").toString())
        );
    }

    @Test
    @ExpectedToFail
    void touchesCacheNot(@Mktmp final Path temp, @RandomProgram final String program)
        throws Exception {
        final Farea farea = new Farea(temp);
        this.executeXmirToPhi(farea, program);
        final File phi = temp.resolve("target/eo/phi/foo.phi").toFile();
        final long modified = phi.lastModified();
        this.executeOnlyXmirToPhi(farea);
        MatcherAssert.assertThat(
            "phi expression file recreated twice",
            modified,
            Matchers.equalTo(phi.lastModified())
        );
    }

    @Test
    void touchesInvalidCache(@Mktmp final Path temp, final @RandomProgram String program)
        throws Exception {
        final Farea farea = new Farea(temp);
        this.executeXmirToPhi(farea, program);
        farea.together(
            f -> {
                final Requisite file = f.files().file("target/eo/2-optimize/foo.xmir");
                file.write(file.content().getBytes());
            }
        );
        final File phi = temp.resolve("target/eo/phi/foo.phi").toFile();
        final long modified = phi.lastModified();
        this.executeOnlyXmirToPhi(farea);
        MatcherAssert.assertThat(
            "xmir-to-phi cache not invalidated",
            modified,
            Matchers.lessThan(phi.lastModified())
        );
    }

    void executeXmirToPhi(final Farea farea, final String program) throws IOException {
        farea.together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(program.getBytes());
                f.build()
                    .plugins()
                    .appendItself()
                    .execution()
                    .goals("register", "parse", "optimize", "xmir-to-phi");
                f.exec("compile");
            }
        );
    }

    void executeOnlyXmirToPhi(final Farea farea) throws IOException {
        farea.together(
            f -> {
                f.build()
                    .plugins()
                    .appendItself()
                    .execution()
                    .goals("xmir-to-phi");
                f.exec("compile");
            }
        );
    }
}
