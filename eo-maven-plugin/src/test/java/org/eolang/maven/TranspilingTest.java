/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import java.nio.file.Paths;
import java.util.Collections;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;

/**
 * Test case for {@link Transpiling}.
 * @since 0.1
 */
final class TranspilingTest {

    @Test
    void foldsInImportedXslLibrariesIntoVersion() {
        MatcherAssert.assertThat(
            "the cache-key version must differ from a fingerprint of the top-level XSLS alone, proving the xsl:import-ed libraries are actually folded in",
            TranspilingTest.transpiling().version(),
            Matchers.not(
                Matchers.equalTo(
                    String.format(
                        "1.0-SNAPSHOT-%s", new Fingerprint(Transpilation.XSLS).get()
                    )
                )
            )
        );
    }

    private static Transpiling transpiling() {
        return new Transpiling(
            Collections.emptyList(),
            Paths.get("target"),
            Paths.get("target/generated"),
            Paths.get("target/cache"),
            true,
            "1.0-SNAPSHOT",
            false,
            Collections.singleton(Paths.get("src/main/java")),
            new Transpilation(
                "1.0-SNAPSHOT",
                new Tracking(false, false),
                false,
                "PhDefault",
                Paths.get("target/xsl-measures.csv"),
                Paths.get("target")
            ),
            new ConcurrentCache()
        );
    }
}
