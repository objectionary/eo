/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import org.eolang.parser.EoSyntax;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Test case for {@link Transpilation}.
 *
 * @since 0.74
 */
@ExtendWith(MktmpResolver.class)
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
    void tellsInferenceTablesApartInTheCacheKey(@Mktmp final Path temp) throws IOException {
        final Path tables = Files.createDirectories(temp.resolve("tables"));
        Files.writeString(tables.resolve("provides.xml"), "<provides><type id='Q.f'/></provides>");
        MatcherAssert.assertThat(
            "a build that read the rows of this object must not take one that had none",
            this.transpilation(tables).version(Collections.singletonList("Q.f")),
            Matchers.not(
                Matchers.equalTo(
                    this.transpilation(temp.resolve("absent"))
                        .version(Collections.singletonList("Q.f"))
                )
            )
        );
    }

    @Test
    void tellsDeadlinesApartInTheCacheKey() {
        MatcherAssert.assertThat(
            "a build whose tests run under one deadline took the result of one whose tests run under another",
            this.transpilation(3L, "1G").version(),
            Matchers.not(Matchers.equalTo(this.transpilation(11L, "1G").version()))
        );
    }

    @Test
    void tellsMemoryBudgetsApartInTheCacheKey() {
        MatcherAssert.assertThat(
            "a build whose tests run under one memory budget took the result of one whose tests run under another",
            this.transpilation(1L, "257M").version(),
            Matchers.not(Matchers.equalTo(this.transpilation(1L, "3G").version()))
        );
    }

    @Test
    void passesGivenDeadlineToGeneratedTimeout(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "The given deadline didnt reach the timeout of a generated test",
            TranspilationTest.tested(this.transpilation(13L, "1G", temp)),
            Matchers.containsString("@Timeout(value = 13, unit = TimeUnit.SECONDS)")
        );
    }

    @Test
    void passesGivenMemoryToGeneratedBudget(@Mktmp final Path temp) throws IOException {
        MatcherAssert.assertThat(
            "The given memory budget didnt reach the budget of a generated test",
            TranspilationTest.tested(this.transpilation(1L, "389M", temp)),
            Matchers.containsString("@Budget(\"389M\")")
        );
    }

    @Test
    void foldsInImportedXslLibrariesIntoVersion() {
        MatcherAssert.assertThat(
            "the cache-key version must differ from a fingerprint of the top-level XSLS alone, proving the xsl:import-ed libraries are actually folded in",
            this.transpilation(new Tracking(false, false)).version(),
            Matchers.not(
                Matchers.startsWith(new Fingerprint(Transpilation.XSLS).get())
            )
        );
    }

    @Test
    void buildsSourceFunctionForParentlessMeasuresPath() {
        Assertions.assertDoesNotThrow(
            () -> new Transpilation(
                new Tracking(false, false),
                false,
                "PhDefault",
                1L,
                "1G",
                Paths.get("xsl-measures.csv"),
                Paths.get("target"),
                Paths.get("target/eo/6-inference")
            ).forSource("foo"),
            "forSource() must not throw when eo.xslMeasuresFile is a bare relative path with no parent directory"
        );
    }

    private Transpilation transpilation(final Tracking tracking) {
        return new Transpilation(
            tracking,
            false,
            "PhDefault",
            1L,
            "1G",
            Paths.get("xsl-measures.csv"),
            Paths.get("target"),
            Paths.get("target/eo/6-inference")
        );
    }

    private Transpilation transpilation(final long deadline, final String memory) {
        return new Transpilation(
            new Tracking(false, false),
            false,
            "PhDefault",
            deadline,
            memory,
            Paths.get("xsl-measures.csv"),
            Paths.get("target"),
            Paths.get("target/eo/6-inference")
        );
    }

    private Transpilation transpilation(
        final long deadline, final String memory, final Path temp
    ) {
        return new Transpilation(
            new Tracking(false, false),
            false,
            "PhDefault",
            deadline,
            memory,
            temp.resolve("xsl-measures.csv"),
            temp.resolve("target"),
            temp.resolve("inference")
        );
    }

    private static String tested(final Transpilation train) throws IOException {
        return String.join(
            "",
            train.forSource("foo").apply(
                new EoSyntax(
                    String.join(
                        System.lineSeparator(),
                        "[] > foo", "  [] +> works", "    true > @", ""
                    )
                ).parsed()
            ).xpath("//tests/text()")
        );
    }

    private Transpilation transpilation(final Path tables) {
        return new Transpilation(
            new Tracking(false, false),
            false,
            "PhDefault",
            1L,
            "1G",
            Paths.get("xsl-measures.csv"),
            Paths.get("target"),
            tables
        );
    }
}
