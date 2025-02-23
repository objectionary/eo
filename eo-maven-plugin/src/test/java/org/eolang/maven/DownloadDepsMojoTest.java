/*
 * The MIT License (MIT)
 *
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import org.cactoos.list.ListOf;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link DownloadDepsMojo}.
 *
 * @since 0.39
 */
@ExtendWith(MktmpResolver.class)
final class DownloadDepsMojoTest {

    /**
     * Path to result output directory.
     */
    private static final Path OUT = Paths.get("target/classes");

    /**
     * Names of all necessary dependencies.
     */
    private static final Collection<String> DEPS_NAMES = new ListOf<>("jna-5.14.0.jar");

    @Test
    void executesWithoutErrors(@Mktmp final Path temp) {
        Assertions.assertDoesNotThrow(
            () -> new FakeMaven(temp).execute(DownloadDepsMojo.class),
            "Exception shouldn't been thrown"
        );
    }

    @Test
    void createsOutDir(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp).execute(DownloadDepsMojo.class);
        Assertions.assertTrue(
            Files.exists(temp.resolve(DownloadDepsMojoTest.OUT)),
            String.format("Expected that \"%s\" target directory exists", DownloadDepsMojoTest.OUT)
        );
    }

    @Test
    void downloadsCorrectly(@Mktmp final Path temp) throws IOException {
        new FakeMaven(temp).execute(DownloadDepsMojo.class);
        for (final String dep : DownloadDepsMojoTest.DEPS_NAMES) {
            Assertions.assertTrue(
                Files.exists(temp.resolve(DownloadDepsMojoTest.OUT).resolve(dep)),
                String.format(
                    "Expected that \"%s\" dependency was downloaded correctly",
                    temp.resolve(DownloadDepsMojoTest.OUT).resolve(dep)
                )
            );
        }
    }
}
