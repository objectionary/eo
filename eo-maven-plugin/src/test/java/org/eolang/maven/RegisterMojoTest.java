/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test cases for {@link RegisterMojo}.
 *
 * @since 0.11
 */
@ExtendWith(MktmpResolver.class)
final class RegisterMojoTest {
    /**
     * Parameter for source directory.
     */
    private static final String PARAM = "sourcesDir";

    /**
     * Source directory.
     */
    private static final String SOURCES = "src/eo";

    @Test
    void registersOkNames(@Mktmp final Path temp) throws IOException {
        new Saved(
            new ResourceOf("org/eolang/maven/file-name/abc-def.eo"),
            temp.resolve("src/eo/org/eolang/maven/abc-def.eo")
        ).value();
        final FakeMaven maven = new FakeMaven(temp)
            .with(RegisterMojoTest.PARAM, temp.resolve(RegisterMojoTest.SOURCES).toFile())
            .execute(new FakeMaven.Register());
        MatcherAssert.assertThat(
            "The resource must exist, but it doesn't",
            maven.foreign().getById("org.eolang.maven.abc-def").exists("id"),
            Matchers.is(true)
        );
    }

    @Test
    void failsWithDotNames(@Mktmp final Path temp) throws IOException {
        new Saved(
            new ResourceOf("org/eolang/maven/file-name/.abc.eo"),
            temp.resolve("src/eo/org/eolang/maven/.abc.eo")
        ).value();
        final IllegalStateException exception = Assertions.assertThrows(
            IllegalStateException.class,
            () -> {
                new FakeMaven(temp)
                    .with(RegisterMojoTest.PARAM, temp.resolve(RegisterMojoTest.SOURCES).toFile())
                    .execute(new FakeMaven.Register());
            }
        );
        MatcherAssert.assertThat(
            "The error message must be correct",
            exception.getCause().getCause().getMessage(),
            Matchers.containsString("Incorrect name found: '.abc.eo'")
        );
    }

    @Test
    void doesNotFailWhenNoStrictNames(@Mktmp final Path temp) throws IOException {
        new Saved(
            new ResourceOf("org/eolang/maven/file-name/.abc.eo"),
            temp.resolve("src/eo/org/eolang/maven/.abc.eo")
        ).value();
        final FakeMaven maven = new FakeMaven(temp)
            .with(RegisterMojoTest.PARAM, temp.resolve(RegisterMojoTest.SOURCES).toFile())
            .with("strictFileNames", false)
            .execute(new FakeMaven.Register());
        MatcherAssert.assertThat(
            "The resource with incorrect id must exist, but it doesn't",
            maven.foreign().getById("org.eolang.maven..abc").exists("id"),
            Matchers.is(true)
        );
    }

    @Test
    void throwsExceptionInCaseSourceDirIsNotSet(@Mktmp final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withoutDefaults()
                .execute(new FakeMaven.Register()),
            String.format(
                "sourcesDir should not be set and the %s should fail, but didn't",
                RegisterMojo.class
            )
        );
    }
}
