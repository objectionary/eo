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
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test cases for {@link PackageInfos}.
 *
 * @since 0.60
 */
@ExtendWith(MktmpResolver.class)
final class PackageInfosTest {

    @Test
    void returnsCorrectNumberOfPackageInfosInSubDirectories(@Mktmp final Path tmp)
        throws IOException {
        final Path subdir = tmp.resolve("subdir");
        Files.createDirectory(subdir);
        Files.createDirectories(subdir.resolve("subsubdir"));
        MatcherAssert.assertThat(
            "We should create exactly two package-info.java files for two subdirectories",
            new PackageInfos(tmp).create(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void createsPackageInfosInSubDirectories(@Mktmp final Path tmp) throws IOException {
        final Path subdir = tmp.resolve("subdir");
        final Path subsubdir = subdir.resolve("subsubdir");
        Files.createDirectory(subdir);
        Files.createDirectories(subsubdir);
        new PackageInfos(tmp).create();
        MatcherAssert.assertThat(
            "package-info.java should be created in the both subdirectories",
            Files.exists(subdir.resolve("package-info.java"))
                && Files.exists(subsubdir.resolve("package-info.java")),
            Matchers.is(true)
        );
    }

    @Test
    void ignoresTheRootDirectoryItself(@Mktmp final Path tmp) throws IOException {
        MatcherAssert.assertThat(
            "No package-info.java files should be created in the root directory",
            new PackageInfos(tmp).create(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void ignoresTheRootDirectoryAndDoesNotCreateFiles(@Mktmp final Path tmp) throws IOException {
        new PackageInfos(tmp).create();
        MatcherAssert.assertThat(
            "package-info.java should not be created in the root directory",
            Files.exists(tmp.resolve("package-info.java")),
            Matchers.is(false)
        );
    }

    @ParameterizedTest
    @CsvSource({
        "invalid-dir, invalid_dir",
        "in valid dir, in_valid_dir",
        "in@valid#dir$, in_valid_dir$",
        "123numericDir, _123numericDir",
        "a.b.c, a.b.c"
    })
    void createsPackageInfoEvenWithWrongSymbols(
        final String dir,
        final String expected,
        @Mktmp final Path tmp
    ) throws IOException {
        final Path subdir = tmp.resolve(dir);
        Files.createDirectory(subdir);
        new PackageInfos(tmp).create();
        MatcherAssert.assertThat(
            "package-info.java should contain correct package name",
            Files.readString(subdir.resolve("package-info.java")),
            Matchers.containsString(String.format("package %s;", expected))
        );
    }
}
