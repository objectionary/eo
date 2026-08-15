/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Paths;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Transpilation}.
 * @since 0.74
 */
final class TranspilationTest {

    @Test
    void buildsSourceFunctionForParentlessMeasuresPath() {
        Assertions.assertDoesNotThrow(
            () -> new Transpilation(
                "1.0-SNAPSHOT",
                new Tracking(false, false),
                new PhiSettings(false, "PhDefault"),
                new Outputs(
                    Paths.get("target"),
                    Paths.get("target/generated"),
                    Paths.get("target/cache"),
                    Paths.get("xsl-measures.csv")
                )
            ).forSource("foo"),
            "forSource() must not throw when eo.xslMeasuresFile is a bare relative path with no parent directory"
        );
    }
}
