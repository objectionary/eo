/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.DisabledOnOs;
import org.junit.jupiter.api.condition.OS;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link EOwin32$EOstat64}.
 *
 * @since 0.77.0
 */
@ExtendWith(MktmpResolver.class)
final class EOwin32EOstat64Test {

    @Test
    @DisabledOnOs({OS.MAC, OS.LINUX})
    void reportsTheFileSizeNotATimestamp(@Mktmp final Path temp) throws IOException {
        final byte[] content = "queried by a distinct, visibly-sized payload".getBytes(
            StandardCharsets.UTF_8
        );
        final Path file = temp.resolve("sized.txt");
        Files.write(file, content);
        MatcherAssert.assertThat(
            "The reported size must be the file's actual byte length, not an epoch timestamp misread from the following field",
            new Dataized(
                new PhApplication(
                    new EOwin32$EOstat64(),
                    new Bind("path", new Data.ToPhi(file.toString()))
                ).take("size")
            ).asNumber().longValue(),
            Matchers.equalTo((long) content.length)
        );
    }

    @Test
    void refusesPathWithNul() {
        MatcherAssert.assertThat(
            "the 'path' attribute carrying a NUL must be refused by name, but it wasnt",
            Assertions.assertThrows(
                ExAbstract.class,
                () -> new Dataized(
                    new PhApplication(
                        new EOwin32$EOstat64(),
                        new Bind(
                            "path",
                            new Data.ToPhi(String.join(String.valueOf((char) 0), "one", "two"))
                        )
                    ).take("size")
                ).take(),
                "a 'path' attribute with a NUL was expected to fail"
            ).getMessage(),
            Matchers.allOf(
                Matchers.containsString("'path' attribute"),
                Matchers.containsString("NUL")
            )
        );
    }
}
