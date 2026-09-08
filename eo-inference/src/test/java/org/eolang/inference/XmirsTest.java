/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.inference;

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
 * Test case for {@link Xmirs}.
 * @since 0.73.4
 */
@ExtendWith(MktmpResolver.class)
final class XmirsTest {

    @Test
    void ignoresADirectoryNamedLikeAnXmirFile(@Mktmp final Path temp) throws IOException {
        final Path dir = Files.createDirectories(temp.resolve("xmirs"));
        Files.writeString(
            dir.resolve("cup.xmir"),
            String.join(
                "",
                "<object><o line='1' loc='Φ.cup' name='cup' pos='0'>",
                "<o line='2' loc='Φ.cup.lid' name='lid' pos='2'/></o></object>"
            )
        );
        Files.createDirectories(dir.resolve("stale.xmir"));
        MatcherAssert.assertThat(
            "a folder whose name ends with .xmir must be left alone, but it was read",
            new Xmirs(dir).formations(),
            Matchers.hasSize(2)
        );
    }
}
