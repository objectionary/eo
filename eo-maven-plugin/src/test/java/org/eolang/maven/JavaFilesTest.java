/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Random;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link JavaFiles}.
 * @since 0.74
 */
@ExtendWith(MktmpResolver.class)
final class JavaFilesTest {

    @Test
    void countsJavaFilesItWrites(@Mktmp final Path temp) throws IOException {
        final long seed = new Random().nextLong();
        final String name = String.format(
            "bär%d.Ωne%d", Math.abs(seed % 97), Math.abs(seed % 13)
        );
        final Path xmir = temp.resolve("main.xmir");
        new Saved(
            String.format(
                "<object><class java-name='%s'><java>class Ω {}</java></class></object>", name
            ),
            xmir
        ).value();
        MatcherAssert.assertThat(
            String.format(
                "the Java files written are not as many as the classes in the XMIR, seed: %d",
                seed
            ),
            new JavaFiles(
                temp.resolve("generated"), temp.resolve("cache").resolve("1.0-SNAPSHOT"), false
            ).total(true, xmir, "", false),
            Matchers.equalTo(1)
        );
    }
}
