/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Optional;
import java.util.regex.PatternSyntaxException;
import org.cactoos.io.ResourceOf;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

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
                .execute(new FakeMaven.Register()).foreign().getById("org.eolang.maven.abc-def")
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
                .with("strictFileNames", false)
                .execute(new FakeMaven.Register()).foreign().getById("org.eolang.maven..abc")
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
                .execute(new FakeMaven.Register()),
            String.format(
                "sourcesDir should not be set and the %s should fail, but didn't",
                MjRegister.class
            )
        );
    }

    @ParameterizedTest
    @ValueSource(strings = {"includeSources", "excludeSources"})
    void failsWithPatternSyntaxExceptionInCaseInvalidGlobs(
        final String prop, @Mktmp final Path temp
    ) {
        MatcherAssert.assertThat(
            String.format(
                "Execution must fail because of %s",
                PatternSyntaxException.class
            ),
            MjRegisterTest.cause(
                Assertions.assertThrows(
                    IllegalStateException.class,
                    () -> new FakeMaven(temp)
                        .with(prop, new SetOf<>("{foo"))
                        .execute(new FakeMaven.Register()),
                    String.format(
                        "%s should fail since supplied globs are invalid",
                        MjRegister.class
                    )
                ),
                PatternSyntaxException.class
            ).isPresent(),
            Matchers.is(true)
        );
    }

    /**
     * Cause.
     * @param err Error
     * @param type Type we are looking for
     * @param <T> Type of expected error
     * @return Throwable
     */
    private static <T extends Throwable> Optional<T> cause(
        final Throwable err,
        final Class<T> type
    ) {
        Optional<T> found = Optional.empty();
        Throwable current = err;
        while (current != null && found.isEmpty()) {
            if (type.isInstance(current)) {
                found = Optional.of(type.cast(current));
            } else {
                current = current.getCause();
            }
        }
        return found;
    }
}
