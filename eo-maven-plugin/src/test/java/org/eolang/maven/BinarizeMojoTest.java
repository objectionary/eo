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

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import org.eolang.maven.rust.Names;
import org.eolang.maven.rust.RustNode;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link BinarizeMojo}.
 *
 * @since 0.1
 */
final class BinarizeMojoTest {

    /**
     * Sources for the tests.
     */
    public static final Path SRC = Paths.get("src/test/resources/org/eolang/maven/binarize/");

    /**
     * BinarizeMojo can binarize without errors.
     * @param temp Temporary directory.
     * @throws Exception If fails.
     */
    @Test
    @Tag("slow")
    @ExtendWith(CargoCondition.class)
    void binarizesWithoutErrors(@TempDir final Path temp) throws Exception {
        final FakeMaven maven;
        synchronized (BinarizeMojoTest.class) {
            maven = new FakeMaven(temp)
                .withProgram(BinarizeMojoTest.SRC.resolve("simple-rust.eo"))
                .withProgram(BinarizeMojoTest.SRC.resolve("twice-rust.eo"));
        }
        Assertions.assertDoesNotThrow(
            () -> maven.execute(new FakeMaven.Binarize())
        );
    }

    @Test
    void failsWithIncorrectInsert(@TempDir final Path temp) throws IOException {
        final Path src = BinarizeMojoTest.SRC.resolve("wrong-rust.eo");
        final FakeMaven maven;
        synchronized (BinarizeMojoTest.class) {
            maven = new FakeMaven(temp)
                .withProgram(src);
        }
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.execute(new FakeMaven.Binarize())
        );
    }

    @Test
    @Tag("slow")
    void savesToCache(@TempDir final Path temp) throws IOException {
        final FakeMaven maven;
        final Path cache = temp.resolve(".cache");
        synchronized (BinarizeMojoTest.class) {
            maven = new FakeMaven(temp)
                .withProgram(BinarizeMojoTest.SRC.resolve("simple-rust.eo"))
                .with("cache", cache);
        }
        final Map<String, Path> res = maven
            .execute(new FakeMaven.Binarize())
            .result();
        MatcherAssert.assertThat(
            res,
            Matchers.hasValue(
                cache.resolve("Lib")
            )
        );
        MatcherAssert.assertThat(
            res,
            Matchers.not(
                Matchers.hasValue(
                    cache.resolve(
                        String.format("Lib/%s0/target/target", Names.PREFIX)
                    )
                )
            )
        );
        Assertions.assertDoesNotThrow(
            () -> maven.execute(new FakeMaven.Binarize())
        );
    }

    @Test
    @Tag("slow")
    void boostsSecondCompilation(@TempDir final Path temp) throws IOException {
        final FakeMaven maven;
        final Path cache = temp.resolve(".cache");
        synchronized (BinarizeMojoTest.class) {
            maven = new FakeMaven(temp)
                .withProgram(BinarizeMojoTest.SRC.resolve("simple-rust.eo"))
                .with("cache", cache);
        }
        long start = System.currentTimeMillis();
        maven.execute(new FakeMaven.Binarize());
        long finish = System.currentTimeMillis();
        final long first = finish - start;
        start = finish;
        maven.execute(new FakeMaven.Binarize());
        finish = System.currentTimeMillis();
        final long second = finish - start;
        MatcherAssert.assertThat(
            second,
            Matchers.lessThan(first)
        );
    }

    @Test
    @Tag("slow")
    void doesNotRecompile(@TempDir final Path temp) throws IOException {
        final FakeMaven maven;
        final Path cache = temp.resolve(".cache");
        synchronized (BinarizeMojoTest.class) {
            maven = new FakeMaven(temp)
                .withProgram(BinarizeMojoTest.SRC.resolve("simple-rust.eo"))
                .with("cache", cache);
        }
        maven.execute(new FakeMaven.Binarize());
        final File executable = cache
            .resolve("Lib/native0/target/debug/")
            .resolve(RustNode.LIB)
            .toFile();
        final long first = executable.lastModified();
        maven.execute(new FakeMaven.Binarize());
        final long second = executable.lastModified();
        MatcherAssert.assertThat(first, Matchers.not(0L));
        MatcherAssert.assertThat(
            second,
            Matchers.equalTo(first)
        );
    }
}
