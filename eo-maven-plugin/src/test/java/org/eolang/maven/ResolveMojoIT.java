/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.manifests.Manifests;
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
 * Integration tests for {@link ResolveMojo}.
 *
 * @since 0.52
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class ResolveMojoIT {
    @Test
    void resolvesJarFile(@Mktmp final Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                ResolveMojoIT.configureFarea(f);
                f.exec("process-classes");
                MatcherAssert.assertThat(
                    "the jar file was resolved and unpacked",
                    f.files().file(
                        String.format(
                            "target/eo/%s/org.eolang/eo-runtime/-/0.39.0/org/eolang/Phi.class",
                            ResolveMojo.DIR
                        )
                    ).exists(),
                    Matchers.is(true)
                );
            }
        );
    }

    @Test
    void removesOldJarFile(@Mktmp final Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                ResolveMojoIT.configureFarea(f);
                f.exec("process-classes");
                f.dependencies()
                    .append("org.eolang", "eo-runtime", "0.40.0");
                f.exec("process-classes");
                MatcherAssert.assertThat(
                    "binary files from the old JAR were removed",
                    f.files().file(
                        String.format(
                            "target/eo/%s/org.eolang/eo-runtime/-/0.39.0",
                            ResolveMojo.DIR
                        )
                    ).exists(),
                    Matchers.is(false)
                );
            }
        );
    }

    private static void configureFarea(final Farea farea) throws IOException {
        farea.clean();
        farea.dependencies()
            .append("org.eolang", "eo-runtime", "0.39.0");
        farea.build()
            .plugins()
            .append(
                "org.eolang",
                "eo-maven-plugin",
                System.getProperty(
                    "eo.version",
                    Manifests.read("EO-Version")
                )
            )
            .execution()
            .goals("resolve");
    }
}
