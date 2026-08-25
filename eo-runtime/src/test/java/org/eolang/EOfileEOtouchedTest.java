/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import com.yegor256.Together;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for the {@code touched} attribute of {@code file}.
 *
 * <p>This is the synchronization-based regression the {@code @todo #7077}
 * in {@code file/touched.eo} asks for. It fails today: the exclusive open
 * landed, but the arm that tolerates {@code EEXIST} reads {@code errno}
 * after {@code linked} has re-run {@code lstat}, so the error code
 * {@code open} set is gone by the time it is compared, and {@code touched}
 * terminates instead of returning the file another thread created.</p>
 *
 * @since 0.75.0
 */
final class EOfileEOtouchedTest {

    /**
     * What the writing thread puts into the shared file.
     */
    private static final byte[] CONTENT = "not empty".getBytes(StandardCharsets.UTF_8);

    @RepeatedTest(20)
    void keepsWhatAnotherThreadWroteInBetween(@TempDir final Path temp) throws IOException {
        final Path target = temp.resolve("shared.txt");
        new Together<>(
            8,
            thread -> {
                if (thread == 0) {
                    Files.write(target, EOfileEOtouchedTest.CONTENT);
                } else {
                    new Dataized(EOfileEOtouchedTest.touched(target)).take();
                }
                return true;
            }
        ).asList();
        MatcherAssert.assertThat(
            "touching a path that another thread created between the probe and the open must leave what it wrote alone, but the file came back truncated",
            Files.size(target),
            Matchers.equalTo((long) EOfileEOtouchedTest.CONTENT.length)
        );
    }

    /**
     * The {@code touched} of the file at this path.
     * @param path The path of the file
     * @return The object to dataize
     */
    private static Phi touched(final Path path) {
        final Phi file = Phi.Φ.take("file").copy();
        file.put(0, new Data.ToPhi(path.toString()));
        return file.take("touched");
    }
}
