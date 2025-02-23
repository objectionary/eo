/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
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
        new HmBase(temp).save(
            new ResourceOf("org/eolang/maven/file-name/abc-def.eo"),
            Paths.get("src/eo/org/eolang/maven/abc-def.eo")
        );
        final FakeMaven maven = new FakeMaven(temp)
            .with(RegisterMojoTest.PARAM, temp.resolve(RegisterMojoTest.SOURCES).toFile())
            .execute(new FakeMaven.Register());
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            maven.foreign().getById("org.eolang.maven.abc-def").exists("id"),
            Matchers.is(true)
        );
    }

    @Test
    void failsWithDotNames(@Mktmp final Path temp) throws IOException {
        new HmBase(temp).save(
            new ResourceOf("org/eolang/maven/file-name/.abc.eo"),
            Paths.get("src/eo/org/eolang/maven/.abc.eo")
        );
        final IllegalStateException exception = Assertions.assertThrows(
            IllegalStateException.class,
            () -> {
                new FakeMaven(temp)
                    .with(RegisterMojoTest.PARAM, temp.resolve(RegisterMojoTest.SOURCES).toFile())
                    .execute(new FakeMaven.Register());
            }
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            exception.getCause().getCause().getMessage(),
            Matchers.containsString("Incorrect name found: '.abc.eo'")
        );
    }

    @Test
    void doesNotFailWhenNoStrictNames(@Mktmp final Path temp) throws IOException {
        new HmBase(temp).save(
            new ResourceOf("org/eolang/maven/file-name/.abc.eo"),
            Paths.get("src/eo/org/eolang/maven/.abc.eo")
        );
        final FakeMaven maven = new FakeMaven(temp)
            .with(RegisterMojoTest.PARAM, temp.resolve(RegisterMojoTest.SOURCES).toFile())
            .with("strictFileNames", false)
            .execute(new FakeMaven.Register());
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
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
