/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
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

/**
 * Test case for {@link ChSource}.
 *
 * @since 0.60
 * @todo #4526:30min Add more tests for {@link ChSource} class.
 *  Currently, there is only one test that checks the hash computation for a file.
 *  We need to add more tests to cover edge cases.
 */
@ExtendWith(MktmpResolver.class)
final class ChSourceTest {

    @Test
    void computesHashValueForTheFile(@Mktmp final Path dir) throws IOException {
        final Path file = dir.resolve("source.eo");
        Files.writeString(
            file,
            "[] > main\n  (stdout \"Hello, EO!\") > @\n"
        );
        MatcherAssert.assertThat(
            "We should compute the correct hash (SHA-1) for the source file",
            new ChSource(file).value(),
            Matchers.equalTo("f97a8da03d184cab9a43e1288293391fb1ccc296")
        );
    }
}
