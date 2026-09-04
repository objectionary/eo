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
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link MjRegister}.
 * @since 0.11
 */
@ExtendWith(MktmpResolver.class)
final class MjRegisterTest {

    @Test
    void registersOkNames(@Mktmp final Path temp) throws IOException {
        new Saved(
            new ResourceOf("org/eolang/maven/file-name/abc-def.eo"),
            temp.resolve("src/eo/org/eolang/maven/abc-def.eo")
        ).value();
        MatcherAssert.assertThat(
            "The resource must exist, but it doesn't",
            new FakeMaven(temp)
                .with("sourcesDir", temp.resolve("src/eo").toFile())
                .execute(new PpRegister()).foreign().getById("org.eolang.maven.abc-def")
                .exists("id"),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotFailWhenNoStrictNames(@Mktmp final Path temp) throws IOException {
        new Saved(
            new ResourceOf("org/eolang/maven/file-name/.abc.eo"),
            temp.resolve("src/eo/org/eolang/maven/.abc.eo")
        ).value();
        MatcherAssert.assertThat(
            "The resource with incorrect id must exist, but it doesn't",
            new FakeMaven(temp)
                .with("sourcesDir", temp.resolve("src/eo").toFile())
                .with("strict", false)
                .execute(new PpRegister()).foreign().getById("org.eolang.maven..abc")
                .exists("id"),
            Matchers.is(true)
        );
    }

    @Test
    void throwsExceptionInCaseSourceDirIsNotSet(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withoutDefaults()
                .execute(new PpRegister()),
            String.format(
                "sourcesDir should not be set and the %s should fail, but didn't",
                MjRegister.class
            )
        );
    }

    @Test
    void forgetsASourceRemovedBetweenTwoRegistrations(@Mktmp final Path temp) throws IOException {
        final Path source = temp.resolve("src/eo/org/eolang/maven/abc-def.eo");
        new Saved(
            new ResourceOf("org/eolang/maven/file-name/abc-def.eo"),
            source
        ).value();
        final FakeMaven maven = new FakeMaven(temp)
            .with("sourcesDir", temp.resolve("src/eo").toFile());
        maven.execute(new PpRegister());
        Files.delete(source);
        MatcherAssert.assertThat(
            "a source deleted between two registrations in one JVM must leave the catalog, but it stayed",
            maven.execute(new PpRegister()).foreign().size(),
            Matchers.equalTo(0)
        );
    }
}
