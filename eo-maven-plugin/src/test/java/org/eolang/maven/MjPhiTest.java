/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.matchers.XhtmlMatchers;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileTime;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.xax.XtSticky;
import org.eolang.xax.XtYaml;
import org.eolang.xax.Xtory;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test cases for {@link MjPhi}.
 * @since 0.34.0
 * @todo #3708:30min Remove @Disabled annotation on
 *  {@code PhiMojoTest.usesCache()} and {@code PhiMojoTest.invalidatesCache()}
 *  when cache is implemented, check that tests is valid otherwise fix them.
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.AvoidDuplicateLiterals"})
@ExtendWith(MktmpResolver.class)
@ExtendWith(RandomProgramResolver.class)
final class MjPhiTest {

    @Test
    void convertsSimpleObjectToPhi(@Mktmp final Path temp, @RandomProgram final String program)
        throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("src/main/eo/foo.eo").write(program.getBytes());
                f.build()
                    .plugins()
                    .appendItself()
                    .execution()
                    .goals("register", "parse", "xmir-to-phi");
                f.exec("compile");
            }
        );
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
                f.files().file("target/eo/1-parse/foo.xmir").write(
                    String.join(
                        " ",
                        "<object>",
                        "<o name='foo'>",
                        "<o name='bar' base='xxx'>",
                        "<o base='org.eolang.bytes'>01-02-03</o>",
                        "</o></o></object>"
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
                f.exec("eo:register", "eo:parse");
                f.exec("eo:xmir-to-phi");
            }
        );
        MatcherAssert.assertThat(
            "the .xmir file is generated",
            XhtmlMatchers.xhtml(
                Files.readString(temp.resolve("target/eo/1-parse/org/eolang/bytes.xmir"))
            ),
            XhtmlMatchers.hasXPaths(
                "/object/o[@name='bytes']",
                "/object/o/o[@base='$.eq']",
                "/object/o/o/o[@base='Q.org.eolang.bytes']/o[text()='01-02-03']"
            )
        );
        MatcherAssert.assertThat(
            "the .phi file is generated",
            Files.readString(temp.resolve("target/eo/phi/org/eolang/bytes.phi")),
            Matchers.allOf(
                Matchers.containsString("yes ↦ eq("),
                Matchers.containsString("Φ̇.bytes(⟦ Δ ⤍ 01-02-03 ⟧)")
            )
        );
    }

    @Test
    void createsFiles(@Mktmp final Path temp) throws Exception {
        MatcherAssert.assertThat(
            String.format(
                "There' should be file with .%s extension after translation to phi, but there isn't",
                MjPhi.EXT
            ),
            new FakeMaven(temp)
                .withProgram(
                    "+package foo.x\n",
                    "# No comments.",
                    "[] > main",
                    "  memory 0 > total",
                    "  [i] > add",
                    "    total.write > @",
                    "      total.plus i"
                )
                .execute(new FakeMaven.Phi())
                .result(),
            Matchers.hasKey(String.format("target/phi/foo/x/main.%s", MjPhi.EXT))
        );
    }

    @Test
    void doesNotFailOnError(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package foo.x\n",
                    "# No comments.",
                    "[] > main",
                    "  seq *-1 > @",
                    "    true"
                )
                .execute(new FakeMaven.Phi()),
            "PhiMojo should not fail on errors"
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/phi-packs", glob = "**.yaml")
    void checksSweetPhiPacks(final String pack, @Mktmp final Path temp) throws Exception {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        final String object = xtory.map().get("object").toString();
        final Place place = new Place(object);
        MatcherAssert.assertThat(
            "must convert to exactly the expression we need with syntax sugar",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(
                        xtory.map().get("input").toString(),
                        object,
                        place.make("eo").toString()
                    )
                    .with("phiNoSugar", false)
                    .execute(new FakeMaven.Phi())
                    .targetPath()
                    .resolve("phi")
                    .resolve(place.make("phi"))
            ).asString(),
            Matchers.equalTo(xtory.map().get("sweet").toString())
        );
    }

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/phi-packs", glob = "**.yaml")
    void checksSaltyPhiPacks(final String pack, @Mktmp final Path temp) throws Exception {
        final Xtory xtory = new XtSticky(new XtYaml(pack));
        Assumptions.assumeTrue(xtory.map().get("skip") == null);
        final String object = xtory.map().get("object").toString();
        final Place place = new Place(object);
        MatcherAssert.assertThat(
            "must convert to exactly the expression we need without syntax sugar",
            new TextOf(
                new FakeMaven(temp)
                    .withProgram(
                        xtory.map().get("input").toString(),
                        object,
                        place.make("eo").toString()
                    )
                    .with("phiNoSugar", true)
                    .execute(new FakeMaven.Phi())
                    .targetPath()
                    .resolve("phi")
                    .resolve(place.make("phi"))
            ).asString(),
            Matchers.equalTo(xtory.map().get("salty").toString())
        );
    }

    @Test
    @Disabled
    void usesCache(@Mktmp final Path temp, @RandomProgram final String program) throws Exception {
        final Path cache = temp.resolve("cache");
        final String hash = "123ZaRiFcHiK321";
        final Path cached = new Saved(
            "some valid phi from cache",
            new CachePath(
                cache.resolve(MjPhi.CACHE),
                FakeMaven.pluginVersion(),
                hash,
                Path.of("foo/x/main.phi")
            ).get()
        ).value();
        Files.setLastModifiedTime(
            cached,
            FileTime.fromMillis(System.currentTimeMillis() + 50_000)
        );
        MatcherAssert.assertThat(
            "Phi is not loaded from cache",
            new TextOf(
                new FakeMaven(temp)
                    .with("cache", cache.toFile())
                    .withProgram(program)
                    .allTojosWithHash(() -> hash)
                    .execute(new FakeMaven.Phi())
                    .result()
                    .get("target/phi/foo/x/main.phi")
            ).asString(),
            Matchers.equalTo(new TextOf(cached).asString())
        );
    }

    @Test
    @Disabled
    void invalidatesCache(
        @Mktmp final Path temp,
        final @RandomProgram String program
    ) throws Exception {
        final Path cache = temp.resolve("cache");
        final String hash = "123ZaRiFcHiK321";
        final File cached = new Saved(
            "some invalid phi (old) from cache",
            new CachePath(
                cache.resolve(MjPhi.CACHE),
                FakeMaven.pluginVersion(),
                hash,
                Path.of("foo/x/main.phi")
            ).get()
        ).value().toFile();
        final long old = cached.lastModified();
        new FakeMaven(temp)
            .with("cache", cache.toFile())
            .withProgram(program)
            .allTojosWithHash(() -> hash)
            .execute(new FakeMaven.Phi());
        MatcherAssert.assertThat(
            "PHI cache not invalidated",
            old,
            Matchers.lessThan(cached.lastModified())
        );
    }

    @Test
    void skips(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("target/eo/1-parse/foo.xmir").write(
                    String.join(
                        "\n",
                        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                        "<object>",
                        "  <metas>",
                        "    <meta>",
                        "      <head>alias</head>",
                        "      <tail>j$foo</tail>",
                        "      <part>j$foo</part>",
                        "    </meta>",
                        "  </metas>",
                        "  <o name='j$foo'>",
                        "    <o name='j$AbstractParent' base='Q.jeo.class'>",
                        "      <o name='j$foo' base='Q.jeo.method'>",
                        "        <o name='signature'>\"\"</o>",
                        "      </o>",
                        "    </o>",
                        "  </o>",
                        "</object>"
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
        final String phi = Files.readString(temp.resolve("target/eo/phi/foo.phi"));
        MatcherAssert.assertThat(
            "PHI expression should not contain 'Φ.j$foo' - j$ names should not get program prefix",
            phi,
            Matchers.not(Matchers.containsString("Φ.j$foo"))
        );
        MatcherAssert.assertThat(
            "PHI should contain 'j$foo ↦ ⟦' as the main object binding",
            phi,
            Matchers.containsString("j$foo ↦ ⟦")
        );
        MatcherAssert.assertThat(
            "PHI should contain 'j$foo ↦ Φ.jeo.method' inside the class",
            phi,
            Matchers.containsString("j$foo ↦ Φ.jeo.method")
        );
    }

    @Test
    void adds(@Mktmp final Path temp) throws Exception {
        new Farea(temp).together(
            f -> {
                f.clean();
                f.files().file("target/eo/1-parse/foo.xmir").write(
                    String.join(
                        "\n",
                        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                        "<object>",
                        "  <metas>",
                        "    <meta>",
                        "      <head>alias</head>",
                        "      <tail>myAlias</tail>",
                        "      <part>myAlias</part>",
                        "    </meta>",
                        "  </metas>",
                        "  <o name='myAlias'>",
                        "    <o name='test' base='string'>hello</o>",
                        "  </o>",
                        "</object>"
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
        final String phi = Files.readString(temp.resolve("target/eo/phi/foo.phi"));
        MatcherAssert.assertThat(
            "PHI should contain 'Φ.myAlias ↦ ⟦' for regular aliases",
            phi,
            Matchers.containsString("Φ.myAlias ↦ ⟦")
        );
    }
}
