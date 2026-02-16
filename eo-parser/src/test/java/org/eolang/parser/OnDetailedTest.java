/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XMLDocument;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests for {@link OnDetailed}.
 *
 * @since 0.56.5
 */
@ExtendWith(MktmpResolver.class)
final class OnDetailedTest {

    @Test
    void reportsMoreClearly(@Mktmp final Path temp) throws IOException {
        Files.write(
            temp.resolve("f.xmir"),
            "<nothing/>".getBytes(StandardCharsets.UTF_8)
        );
        Assertions.assertThrows(
            Exception.class,
            () -> new OnDetailed(
                new OnDefault(new XMLDocument("<nothing/>")), temp.resolve("f.xmir")
            ).get(),
            "Exception was not thrown, but it should, since object name is not here"
        );
    }
}
