/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link UnspileMojo}.
 *
 * @since 0.1
 */
@ExtendWith(MktmpResolver.class)
final class UnspileMojoTest {

    @Test
    void cleans(@Mktmp final Path temp) throws IOException {
        final Path foo = Paths.get("a/b/c/foo.class");
        final FakeMaven maven = new FakeMaven(temp);
        new Saved("abc", temp.resolve(foo)).value();
        new Saved("xxx", maven.generatedPath().resolve("a/b/c/foo.java")).value();
        new Saved("cde", maven.targetPath().resolve("classes/foo.txt")).value();
        maven.execute(UnspileMojo.class);
        MatcherAssert.assertThat(
            String.format("UnspileMojo unable to remove %s class", foo),
            Files.exists(foo),
            Matchers.is(false)
        );
    }

    @Test
    void doesNotDeleteClassIfPresentInSources(@Mktmp final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        new Saved("src", temp.resolve("src/main/java/EOorg/package-info.java")).value();
        new Saved("gen1", maven.generatedPath().resolve("EOorg/package-info.java")).value();
        new Saved(
            "gen2", maven.generatedPath().resolve("EOorg/EOeolang/package-info.java")
        ).value();
        final Path org = new Saved(
            "cls", temp.resolve("target/classes/EOorg/package-info.class")
        ).value();
        final Path eolang = new Saved(
            "cls", temp.resolve("target/classes/EOorg/EOeolang/package-info.class")
        ).value();
        maven.execute(UnspileMojo.class);
        MatcherAssert.assertThat(
            "UnspileMojo must not delete classes if corresponding .java files exist in java sources directory",
            new Walk(temp.resolve("target/classes")),
            Matchers.allOf(
                Matchers.hasItem(org),
                Matchers.not(Matchers.hasItem(eolang))
            )
        );
    }
}
