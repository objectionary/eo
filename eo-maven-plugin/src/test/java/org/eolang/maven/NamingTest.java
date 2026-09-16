/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.Map;
import java.util.Random;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Naming}.
 *
 * @since 0.74
 */
@ExtendWith(MktmpResolver.class)
final class NamingTest {

    @Test
    void rejectsFileSystemThatCannotCarryObjectNames(@Mktmp final Path temp) throws IOException {
        final long seed = new Random().nextLong();
        try (FileSystem ascii = FileSystems.newFileSystem(
            temp.resolve("ascii.zip"), Map.of("create", "true", "encoding", "US-ASCII")
        )) {
            Assertions.assertThrows(
                IllegalStateException.class,
                new Naming(ascii, String.format("EOΦ%dпривет", seed))::exec,
                String.format(
                    "a file system in ASCII passed as able to name EO objects, seed: %d", seed
                )
            );
        }
    }
}
