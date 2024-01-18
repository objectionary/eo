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

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.io.ResourceOf;
import org.eolang.maven.util.HmBase;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test cases for {@link RegisterMojo}.
 *
 * @since 0.11
 */
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
    void registersOkNames(@TempDir final Path temp) throws IOException {
        new HmBase(temp).save(
            new ResourceOf("org/eolang/maven/file-name/abc-def.eo"),
            Paths.get("src/eo/org/eolang/maven/abc-def.eo")
        );
        final FakeMaven maven = new FakeMaven(temp)
            .with(RegisterMojoTest.PARAM, temp.resolve(RegisterMojoTest.SOURCES).toFile())
            .execute(new FakeMaven.Register());
        MatcherAssert.assertThat(
            maven.foreign().getById("org.eolang.maven.abc-def").exists("id"),
            Matchers.is(true)
        );
    }

    @Test
    void failsWithDotNames(@TempDir final Path temp) throws IOException {
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
            exception.getCause().getCause().getMessage(),
            Matchers.containsString("Incorrect name found: '.abc.eo'")
        );
    }

    @Test
    void doesNotFailWhenNoStrictNames(@TempDir final Path temp) throws IOException {
        new HmBase(temp).save(
            new ResourceOf("org/eolang/maven/file-name/.abc.eo"),
            Paths.get("src/eo/org/eolang/maven/.abc.eo")
        );
        final FakeMaven maven = new FakeMaven(temp)
            .with(RegisterMojoTest.PARAM, temp.resolve(RegisterMojoTest.SOURCES).toFile())
            .with("strictFileNames", false)
            .execute(new FakeMaven.Register());
        MatcherAssert.assertThat(
            maven.foreign().getById("org.eolang.maven..abc").exists("id"),
            Matchers.is(true)
        );
    }

    @Test
    void throwsExceptionInCaseSourceDirIsNotSet(@TempDir final Path temp) {
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
