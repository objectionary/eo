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
package org.eolang.maven.tojos;

import com.yegor256.tojos.MnJson;
import com.yegor256.tojos.MnSticky;
import com.yegor256.tojos.TjCached;
import com.yegor256.tojos.TjDefault;
import com.yegor256.tojos.Tojos;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Tests for {@link TranspiledTojos}.
 *
 * @since 0.30
 */
final class TranspiledTojosTest {

    /**
     * Transpiled files.
     */
    private List<Path> transpiled;

    /**
     * Temporary directory.
     */
    private Path temp;

    /**
     * Original tojos.
     */
    private Tojos original;

    /**
     * Testable transpiled tojos.
     */
    private TranspiledTojos tojos;

    @BeforeEach
    void setUp(@TempDir final Path tmp) throws IOException {
        this.temp = tmp;
        this.transpiled = Stream.of("a", "b", "c")
            .map(tmp::resolve)
            .collect(Collectors.toList());
        for (final Path path : this.transpiled) {
            Files.write(path, "test".getBytes(StandardCharsets.UTF_8));
        }
        this.original = new TjCached(
            new TjDefault(
                new MnSticky(
                    new MnJson(tmp.resolve("tojos").resolve("transpiled.csv"))
                )
            )
        );
        this.tojos = new TranspiledTojos(this.original);
    }

    @Test
    void adds() {
        this.tojos.add(this.transpiled.get(0), Paths.get("first.optimized.xmir"));
        MatcherAssert.assertThat(
            this.original.select(all -> true),
            Matchers.hasSize(1)
        );
    }

    @Test
    void removesExistingTranspiledFiles() {
        final Path first = Paths.get("1.optimized.xmir");
        final Path second = Paths.get("2.optimized.xmir");
        this.tojos.add(this.transpiled.get(0), Paths.get("0.optimized.xmir"));
        this.tojos.add(this.transpiled.get(1), first);
        this.tojos.add(this.transpiled.get(2), second);
        MatcherAssert.assertThat(
            this.tojos.remove(first),
            Matchers.equalTo(1L)
        );
        MatcherAssert.assertThat(
            this.tojos.remove(second),
            Matchers.equalTo(1L)
        );
        MatcherAssert.assertThat(
            this.temp.toFile().listFiles(File::isFile),
            Matchers.arrayWithSize(1)
        );
        MatcherAssert.assertThat(
            this.original.select(all -> true),
            Matchers.hasSize(3)
        );
    }

    @Test
    void removesAbsent() {
        MatcherAssert.assertThat(
            this.tojos.remove(Paths.get("absent.xmir")),
            Matchers.equalTo(0L)
        );
        MatcherAssert.assertThat(
            this.temp.toFile().listFiles(File::isFile),
            Matchers.arrayWithSize(3)
        );
        MatcherAssert.assertThat(
            this.original.select(all -> true),
            Matchers.hasSize(0)
        );
    }

    @AfterEach
    void tearDown() throws IOException {
        this.tojos.close();
    }
}
