/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
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
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests for {@link TjsTranspiled}.
 *
 * @since 0.30
 */
@ExtendWith(MktmpResolver.class)
final class TjsTranspiledTest {

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
    private TjsTranspiled tojos;

    @BeforeEach
    void setUp(@Mktmp final Path tmp) throws IOException {
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
        this.tojos = new TjsTranspiled(this.original);
    }

    @Test
    void adds() {
        this.tojos.add(this.transpiled.get(0), Paths.get("first.optimized.xmir"));
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
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
            CatalogsTest.TO_ADD_MESSAGE,
            this.tojos.remove(first),
            Matchers.equalTo(1L)
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            this.tojos.remove(second),
            Matchers.equalTo(1L)
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            this.temp.toFile().listFiles(File::isFile),
            Matchers.arrayWithSize(1)
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            this.original.select(all -> true),
            Matchers.hasSize(3)
        );
    }

    @Test
    void removesAbsent() {
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            this.tojos.remove(Paths.get("absent.xmir")),
            Matchers.equalTo(0L)
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            this.temp.toFile().listFiles(File::isFile),
            Matchers.arrayWithSize(3)
        );
        MatcherAssert.assertThat(
            CatalogsTest.TO_ADD_MESSAGE,
            this.original.select(all -> true),
            Matchers.hasSize(0)
        );
    }

    @AfterEach
    void tearDown() throws IOException {
        this.tojos.close();
    }
}
