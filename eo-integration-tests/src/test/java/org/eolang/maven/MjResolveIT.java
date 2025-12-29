/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for eo-maven-plugin:resolve.
 *
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class MjResolveIT {

    @Test
    void resolvesJarFile(@Mktmp final Path temp) throws IOException {
        final String version = "0.39.0";
        new Farea(temp).together(
            f -> {
                MjResolveIT.configureFarea(f, version);
                f.exec("process-classes");
                MatcherAssert.assertThat(
                    "the jar file was resolved and unpacked",
                    f.files().file(
                        String.format(
                            "target/eo/%s/org.eolang/eo-runtime/-/%s/org/eolang/Phi.class",
                            "4-resolve",
                            version
                        )
                    ).exists(),
                    Matchers.is(true)
                );
            }
        );
    }

    @Test
    void logsJarWithEmptyClassifierCorrectly(@Mktmp final Path temp) throws IOException {
        final String version = "0.52.0";
        new Farea(temp).together(
            f -> {
                MjResolveIT.configureFarea(f, version);
                f.exec("process-classes");
                MatcherAssert.assertThat(
                    "Classifier should not be displayed, if its absent",
                    f.log().content(),
                    Matchers.containsString(
                        String.format(
                            "org.eolang:eo-runtime:%s unpacked to",
                            version
                        )
                    )
                );
            }
        );
    }

    @Test
    void removesOldJarFile(@Mktmp final Path temp) throws IOException {
        final String version = "0.38.0";
        new Farea(temp).together(
            f -> {
                MjResolveIT.configureFarea(f, version);
                f.exec("process-classes");
                f.dependencies()
                    .append("org.eolang", "eo-runtime", "0.40.0");
                f.exec("process-classes");
                MatcherAssert.assertThat(
                    "binary files from the old JAR were removed",
                    f.files().file(
                        String.format(
                            "target/eo/%s/org.eolang/eo-runtime/-/%s",
                            "4-resolve",
                            version
                        )
                    ).exists(),
                    Matchers.is(false)
                );
            }
        );
    }

    private static void configureFarea(final Farea farea, final String version) throws IOException {
        farea.clean();
        farea.dependencies()
            .append("org.eolang", "eo-runtime", version);
        new AppendedPlugin(farea).value()
            .goals("resolve");
    }
}
