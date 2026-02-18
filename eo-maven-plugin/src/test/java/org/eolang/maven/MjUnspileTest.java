/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Set;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link MjUnspile}.
 *
 * @since 0.1
 */
@ExtendWith(MktmpResolver.class)
@SuppressWarnings("PMD.UnnecessaryLocalRule")
final class MjUnspileTest {

    @Test
    void deletesClassIfCompiledFromGeneratedSources(@Mktmp final Path temp) throws IOException {
        final Path foo = Paths.get("a/b/c/foo.class");
        final FakeMaven maven = new FakeMaven(temp);
        new Saved("abc", temp.resolve(foo)).value();
        new Saved("xxx", maven.generatedPath().resolve("a/b/c/foo.java")).value();
        maven.execute(MjUnspile.class);
        MatcherAssert.assertThat(
            String.format("UnspileMojo unable to remove %s class", foo),
            Files.exists(foo),
            Matchers.is(false)
        );
    }

    @Test
    void keepsSpecifiedClasses(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp)
            .with("keepBinaries", Set.of("EOorg/package-info.class"));
        new Saved("gen1", maven.generatedPath().resolve("EOorg/package-info.java")).value();
        new Saved(
            "gen2", maven.generatedPath().resolve("EOorg/EOeolang/package-info.java")
        ).value();
        final Path org = new Saved(
            "clz", maven.classesPath().resolve("EOorg/package-info.class")
        ).value();
        final Path eolang = new Saved(
            "pkg", maven.classesPath().resolve("EOorg/EOeolang/package-info.class")
        ).value();
        MatcherAssert.assertThat(
            "UnspileMojo must keep files matching to keepBinaries globs",
            maven.execute(MjUnspile.class).result(),
            Matchers.allOf(
                Matchers.hasValue(org),
                Matchers.not(Matchers.hasValue(eolang))
            )
        );
    }

    @Test
    void deletesInnerGeneratedClasses(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        new Saved("outer", maven.generatedPath().resolve("EOorg/EOnumber.java")).value();
        final Path clazz = new Saved(
            "clz", maven.classesPath().resolve("EOorg/EOnumber.class")
        ).value();
        final Path inner = new Saved(
            "inner", maven.classesPath().resolve("EOorg/EOnumber$1$2$3.class")
        ).value();
        final Path located = new Saved(
            "clss", maven.classesPath().resolve("EOorg/EOnumber$EOΦorgeolanginner.class")
        ).value();
        MatcherAssert.assertThat(
            "UnspileMojo must delete inner auto generated classes",
            maven.execute(MjUnspile.class).result(),
            Matchers.allOf(
                Matchers.not(Matchers.hasValue(clazz)),
                Matchers.not(Matchers.hasValue(inner)),
                Matchers.not(Matchers.hasValue(located))
            )
        );
    }

    @Test
    void deletesEmptyDirectories(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        new Saved("src", maven.generatedPath().resolve("org/eolang/number.java")).value();
        new Saved("cnt", maven.classesPath().resolve("org/eolang/number.class")).value();
        MatcherAssert.assertThat(
            "UnspileMojo must remove all empty directories recursively",
            maven.execute(MjUnspile.class).result(),
            Matchers.allOf(
                Matchers.not(Matchers.hasKey("target/classes/org/eolang")),
                Matchers.not(Matchers.hasKey("target/classes/org")),
                Matchers.hasKey("target/classes")
            )
        );
    }
}
