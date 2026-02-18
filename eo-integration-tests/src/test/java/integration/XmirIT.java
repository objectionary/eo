/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.jcabi.xml.XMLDocument;
import com.yegor256.MayBeSlow;
import com.yegor256.WeAreOnline;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import org.eolang.parser.StrictXmir;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test for checking validity of parsed EO as XMIR documents.
 *
 * @since 0.58.3
 */
@SuppressWarnings({
    "JTCOP.RuleAllTestsHaveProductionClass",
    "PMD.UnitTestShouldIncludeAssert"
})
final class XmirIT {

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void validatesWithXsd() throws IOException {
        Files.walk(
            Paths.get("").toAbsolutePath().getParent()
                .resolve("eo-runtime")
                .resolve("target")
                .resolve("eo")
                .resolve("1-parse")
            )
            .filter(Files::isRegularFile)
            .forEach(
                xmir -> {
                    try {
                        Assertions.assertDoesNotThrow(
                            new StrictXmir(new XMLDocument(xmir))::inner,
                            "validation should pass as normal"
                        );
                    } catch (final FileNotFoundException exception) {
                        throw new IllegalStateException(
                            String.format("Failed to find XMIR for %s", xmir), exception
                        );
                    }
                }
            );
    }
}
