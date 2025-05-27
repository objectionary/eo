/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
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
import java.io.File;
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
import org.eolang.parser.EoSyntax;
import org.eolang.parser.StrictXmir;
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

/**
 * Test cases for {@link MjUnphi}.
 * @since 0.34.0
 * @todo #4052:35min Replace usage of XML.nodes() with Xnav.path().
 *  For now, in {@link MjUnphiTest#checksUnphiPacks} we still use {@link XML#nodes(String)}, and
 *  cannot replace it with more faster alternative from Xnav, because of
 *  <a href="https://github.com/volodya-lombrozo/xnav/issues/118">this</a>
 *  issue. When it will be resolved, replace the usage to Xnav method.
 */
@SuppressWarnings("PMD.TooManyMethods")
@ExtendWith(MktmpResolver.class)
final class MjUnphiTest {

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
            XhtmlMatchers.hasXPaths("/object/o[1]/o[1][text()='00-']")
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
        new Saved(
            "{⟦std ↦ [[@ -> Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x]]⟧}",
            temp.resolve("target/phi/std.phi")
        ).value();
        MatcherAssert.assertThat(
            String.format(
                "There should be file with .%s extension after parsing phi to XMIR, but there isn't",
                MjAssemble.XMIR
            ),
            new FakeMaven(temp)
                .execute(MjUnphi.class)
                .result(),
            Matchers.hasKey(String.format("target/%s/std.xmir", MjParse.DIR))
        );
    }

    @Test
    void failsIfParsedWithErrors(@Mktmp final Path temp) throws IOException {
        new Saved(
            "std ↦ Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x",
            temp.resolve("target/phi/std.phi")
        ).value();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .execute(MjUnphi.class),
            "UnphiMojo execution should fail because of parsing errors"
        );
    }

    @Test
    void addsMetas(@Mktmp final Path temp) throws IOException {
        new Saved(
            "{⟦std ↦ Φ.org.eolang.io.stdout⟧}",
            temp.resolve("target/phi/std.phi")
        ).value();
        MatcherAssert.assertThat(
            "Unphied XMIR must contain metas, added via \"unphiMetas\" parameter",
            new XMLDocument(
                new FakeMaven(temp)
                    .with(
                        "unphiMetas",
                        new SetOf<>("+tests", "+home https://github.com/objectionary/eo")
                    )
                    .execute(MjUnphi.class)
                    .result()
                    .get(String.format("target/%s/std.xmir", MjParse.DIR))
            ),
            XhtmlMatchers.hasXPaths(
                "/object/metas/meta[head/text()='tests' and not(tail/text())]",
                "/object/metas/meta[head/text()='home' and tail/text()='https://github.com/objectionary/eo']"
            )
        );
    }

    @Test
    void failsIfPackageMetaIsAdded(@Mktmp final Path temp) throws IOException {
        new Saved(
            "{⟦std ↦ Φ.org.eolang.io.stdout⟧}",
            temp.resolve("target/phi/std.phi")
        ).value();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .with("unphiMetas", new SetOf<>("+package org.eolang"))
                .execute(MjUnphi.class),
            "UnphiMojo execution should fail if \"+package\" meta is added"
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/unphi-packs", glob = "**.yaml")
    @SuppressWarnings("unchecked")
    void checksUnphiPacks(final String pack, @Mktmp final Path temp) throws Exception {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        final String phi = xtory.map().get("phi").toString();
        new Saved(phi, temp.resolve("target/phi/main.phi")).value();
        final List<String> failures = new ListOf<>();
        new FakeMaven(temp).execute(MjUnphi.class);
        final XML doc = new StrictXmir(
            new XMLDocument(
                new TextOf(
                    temp.resolve(
                        Paths.get(String.format("target/%s/main.xmir", MjParse.DIR))
                    )
                ).asString()
            )
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
    @ClasspathSource(value = "org/eolang/maven/phi-packs", glob = "**.yaml")
    void convertsToXmirAndBack(final String pack, @Mktmp final Path temp) throws Exception {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        final String phi = xtory.map().get("sweet").toString();
        final String main = "target/phi/main.phi";
        final Path path = temp.resolve(main);
        new Saved(phi, path).value();
        final long saved = path.toFile().lastModified();
        final FakeMaven maven = new FakeMaven(temp).execute(MjUnphi.class);
        final Path xmir = temp.resolve(String.format("target/%s/main.xmir", MjParse.DIR));
        maven.foreignTojos().add("name").withXmir(xmir);
        final Path result = maven
            .with("conservative", xtory.map().get("conservative") != null)
            .execute(MjPhi.class)
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

    @Test
    void convertsValidXmirAndParsableEO(@Mktmp final Path temp)
        throws Exception {
        final Map<String, Path> map = new FakeMaven(temp)
            .withProgram(
                "+package foo.x\n",
                "# No comments.",
                "[args] > main",
                "  QQ.io.stdout > @",
                "    \"Hello, world!\"",
                "  args.@ > phi!",
                "  * (* 1 2) (* 3 4) (* 5 6) > mx"
            )
            .with("printSourcesDir", temp.resolve("target/1-parse").toFile())
            .with("printOutputDir", temp.resolve("target/generated-sources").toFile())
            .execute(MjParse.class)
            .execute(MjPhi.class)
            .execute(MjUnphi.class)
            .execute(MjPrint.class)
            .result();
        MatcherAssert.assertThat(
            "Result EO code should be parsable",
            new EoSyntax(
                new InputOf(
                    new TextOf(
                        temp.resolve(
                            map.get("target/generated-sources/foo/x/main.eo")
                        )
                    )
                )
            ).parsed(),
            XhtmlMatchers.hasXPath("/object[not(errors)]")
        );
    }

    @Test
    void usesCache(@Mktmp final Path temp) throws Exception {
        new Saved(
            "{⟦std ↦ Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x⟧}",
            temp.resolve("target/eo/phi/std.phi")
        ).value();
        final String hash = "123ZaRiFcHiK321";
        final Path cache = temp.resolve("cache");
        final String expected = "some valid XMIR from cache";
        final File cached = new Saved(
            expected,
            new CachePath(
                cache.resolve("unphied"),
                FakeMaven.pluginVersion(),
                hash,
                Path.of("std.xmir")
            ).get()
        ).value().toFile();
        MatcherAssert.assertThat(
            "The cached file's last modified timestamp should be strictly after the source one",
            cached.setLastModified(cached.lastModified() + 1),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            "XMIR file is not loaded from cache",
            new TextOf(
                new FakeMaven(temp)
                    .with("cache", cache.toFile())
                    .with("unphiInputDir", temp.resolve("target/eo/phi/").toFile())
                    .with("unphiOutputDir", temp.resolve("target/eo/1-parse").toFile())
                    .with("hash", new CommitHash.ChConstant(hash))
                    .execute(MjUnphi.class)
                    .result()
                    .get("target/eo/1-parse/std.xmir")
            ).asString(),
            Matchers.equalTo(expected)
        );
    }

    @Test
    void invalidatesCache(@Mktmp final Path temp) throws Exception {
        final String hash = "123ZaRiFcHiK321";
        final Path cache = temp.resolve("cache");
        final File cached = new Saved(
            "some invalid (old) XMIR from cache",
            new CachePath(
                cache.resolve("unphied"),
                FakeMaven.pluginVersion(),
                hash,
                Path.of("std.xmir")
            ).get()
        ).value().toFile();
        new Saved(
            "{⟦std ↦ [[@ -> Φ.org.eolang.io.stdout, y ↦ Φ.org.eolang.x]]⟧}",
            temp.resolve("target/eo/phi/std.phi")
        ).value();
        final long old = cached.lastModified();
        new FakeMaven(temp)
            .with("cache", cache.toFile())
            .with("unphiInputDir", temp.resolve("target/eo/phi/").toFile())
            .with("unphiOutputDir", temp.resolve("target/eo/1-parse").toFile())
            .with("hash", new CommitHash.ChConstant(hash))
            .execute(MjUnphi.class);
        MatcherAssert.assertThat(
            "XMIR cache not invalidated",
            old,
            Matchers.lessThan(cached.lastModified())
        );
    }
}
