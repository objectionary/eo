/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Transpilation}.
 * @since 0.74
 */
final class TranspilationTest {

    @Test
    void tellsTrackedStepsApartInTheCacheKey() {
        MatcherAssert.assertThat(
            "a build that writes the XMIRs of the train must not take the result of one that didnt",
            this.transpilation(new Tracking(true, false)).version(),
            Matchers.not(
                Matchers.equalTo(this.transpilation(new Tracking(false, false)).version())
            )
        );
    }

    @Test
    void buildsSourceFunctionForParentlessMeasuresPath() {
        Assertions.assertDoesNotThrow(
            () -> new Transpilation(
                "1.0-SNAPSHOT",
                new Tracking(false, false),
                false,
                "PhDefault",
                Paths.get("xsl-measures.csv"),
                Paths.get("target"),
                Paths.get("target/eo/6-inference")
            ).forSource("foo"),
            "forSource() must not throw when eo.xslMeasuresFile is a bare relative path with no parent directory"
        );
    }

    private Transpilation transpilation(final Tracking tracking) {
        return new Transpilation(
            "1.0-SNAPSHOT",
            tracking,
            false,
            "PhDefault",
            Paths.get("xsl-measures.csv"),
            Paths.get("target"),
            Paths.get("target/eo/6-inference")
        );
    }
}
