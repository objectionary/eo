/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/**
 * Test cases for {@link PackageInfos}.
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
            new PackageInfos(
                tmp, Collections.emptyList(), Arrays.asList(subdir, subdir.resolve("subsubdir"))
            ).create(),
            Matchers.equalTo(2)
        );
    }

    @Test
    void namesTheRootPackageWithAnEmptyValue(@Mktmp final Path tmp) throws IOException {
        final Path eolang = tmp.resolve("org").resolve("eolang");
        Files.createDirectories(eolang);
        new PackageInfos(tmp, Collections.emptyList(), Collections.singleton(eolang)).create();
        MatcherAssert.assertThat(
            "The root EO package is not the empty name the annotation must carry",
            Files.readString(tmp.resolve("org").resolve("eolang").resolve("package-info.java")),
            Matchers.containsString("@org.eolang.XmirPackage(\"\")")
        );
    }

    @Test
    void skipsPackageInfoWhenHandWrittenOneExists(@Mktmp final Path tmp) throws IOException {
        final Path generated = tmp.resolve("generated");
        final Path handwritten = tmp.resolve("java");
        Files.createDirectories(generated.resolve("EO_привет"));
        Files.createDirectories(handwritten.resolve("EO_привет"));
        Files.write(
            handwritten.resolve("EO_привет").resolve("package-info.java"),
            "package EO_привет;".getBytes(StandardCharsets.UTF_8)
        );
        new PackageInfos(
            generated,
            Collections.singleton(handwritten),
            Collections.singleton(generated.resolve("EO_привет"))
        ).create();
        MatcherAssert.assertThat(
            "package-info.java is generated for a package that already has a hand-written one",
            Files.exists(generated.resolve("EO_привет").resolve("package-info.java")),
            Matchers.is(false)
        );
    }

    @Test
    void createsPackageInfosInSubDirectories(@Mktmp final Path tmp) throws IOException {
        final Path subdir = tmp.resolve("subdir");
        final Path subsubdir = subdir.resolve("subsubdir");
        Files.createDirectory(subdir);
        Files.createDirectories(subsubdir);
        new PackageInfos(
            tmp, Collections.emptyList(), Arrays.asList(subdir, subsubdir)
        ).create();
        MatcherAssert.assertThat(
            "package-info.java should be created in the both subdirectories",
            Files.exists(subdir.resolve("package-info.java"))
                && Files.exists(subsubdir.resolve("package-info.java")),
            Matchers.is(true)
        );
    }

    @Test
    void skipsADirectoryThisRunWroteNothingInto(@Mktmp final Path tmp) throws IOException {
        final Path foreign = tmp.resolve("annotations").resolve("com").resolve("acme");
        Files.createDirectories(foreign);
        new PackageInfos(tmp, Collections.emptyList(), Collections.emptyList()).create();
        MatcherAssert.assertThat(
            "a directory another generator owns must not get a package annotation of ours",
            Files.exists(foreign.resolve("package-info.java")),
            Matchers.is(false)
        );
    }

    @Test
    void ignoresTheRootDirectoryItself(@Mktmp final Path tmp) throws IOException {
        MatcherAssert.assertThat(
            "No package-info.java files should be created in the root directory",
            new PackageInfos(
                tmp, Collections.emptyList(), Collections.singleton(tmp)
            ).create(),
            Matchers.equalTo(0)
        );
    }

    @Test
    void ignoresTheRootDirectoryAndDoesNotCreateFiles(@Mktmp final Path tmp) throws IOException {
        new PackageInfos(tmp, Collections.emptyList(), Collections.singleton(tmp)).create();
        MatcherAssert.assertThat(
            "package-info.java should not be created in the root directory",
            Files.exists(tmp.resolve("package-info.java")),
            Matchers.is(false)
        );
    }

    @Test
    void keepsEoInsideAnObjectsOwnName(@Mktmp final Path tmp) throws IOException {
        final Path own = tmp.resolve("org").resolve("eolang").resolve("EO_xEOy");
        Files.createDirectories(own);
        new PackageInfos(tmp, Collections.emptyList(), Collections.singleton(own)).create();
        MatcherAssert.assertThat(
            "only the leading EO_ transpiler prefix should be stripped, not the EO inside xEOy",
            Files.readString(
                tmp.resolve("org").resolve("eolang").resolve("EO_xEOy")
                    .resolve("package-info.java")
            ),
            Matchers.containsString("@org.eolang.XmirPackage(\"xEOy\")")
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
        new PackageInfos(tmp, Collections.emptyList(), Collections.singleton(subdir)).create();
        MatcherAssert.assertThat(
            "package-info.java should contain correct package name",
            Files.readString(subdir.resolve("package-info.java")),
            Matchers.containsString(String.format("package %s;", expected))
        );
    }
}
