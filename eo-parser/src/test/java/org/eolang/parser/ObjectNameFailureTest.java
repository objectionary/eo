/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.parser;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
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
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Tests for {@link ObjectNameFailure}.
 *
 * @since 0.56.5
 */
@ExtendWith(MktmpResolver.class)
final class ObjectNameFailureTest {

    @Test
    void reportsMoreClearly(@Mktmp final Path temp) throws IOException {
        final XML xmir = new XMLDocument("<nothing/>");
        Files.write(temp.resolve("f.xmir"), xmir.toString().getBytes(StandardCharsets.UTF_8));
        final String expected = "Boom!";
        MatcherAssert.assertThat(
            "Exception message is not detailed, as it should be",
            Assertions.assertThrows(
                Exception.class,
                () -> new ObjectNameFailure(
                    new ObjectName(xmir), input -> {
                        throw new IllegalStateException(expected, input);
                    }
                ).get(),
                "Exception was not thrown, but it should, since object name is not here"
            ).getLocalizedMessage(),
            Matchers.equalTo(expected)
        );
    }
}
