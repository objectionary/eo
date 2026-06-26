/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.jcabi.manifests.Manifests;
import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration tests for transpile behavior.
 * @since 0.62
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@ExtendWith(MktmpResolver.class)
final class TranspileIT {

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void generatesTestSourcesForUserWrittenTests(final @Mktmp Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                f.properties()
                    .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
                    .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
                f.files()
                    .file("src/main/eo/simple.eo")
                    .write(
                        String.join(
                            System.lineSeparator(),
                            "# Simple.",
                            "[] > simple",
                            "  \"hello\" > @",
                            "  [] +> tests-simple-works",
                            "    true > @"
                        ).getBytes(StandardCharsets.UTF_8)
                    );
                f.dependencies().append(
                    "org.eolang",
                    "eo-runtime",
                    System.getProperty("eo.version", Manifests.read("EO-Version"))
                );
                new EoMavenPlugin(f)
                    .appended()
                    .execution("compile")
                    .goals("register", "compile", "transpile")
                    .configuration()
                    .set("failOnWarning", Boolean.FALSE.toString())
                    .set("skipLinting", Boolean.TRUE.toString());
                f.exec("clean", "compile");
            }
        );
        final List<String> names;
        try (Stream<Path> walk = Files.walk(temp.resolve("target/generated-test-sources"))) {
            names = walk
                .filter(Files::isRegularFile)
                .map(p -> p.getFileName().toString())
                .collect(Collectors.toList());
        }
        MatcherAssert.assertThat(
            "User-written test sources must be generated",
            names,
            Matchers.hasItem("EOsimpleTest.java")
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void doesNotGenerateRuntimeTestSourcesForUserProject(final @Mktmp Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                f.properties()
                    .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
                    .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
                f.files()
                    .file("src/main/eo/simple.eo")
                    .write(
                        String.join(
                            System.lineSeparator(),
                            "# Simple.",
                            "[] > simple",
                            "  \"hello\" > @"
                        ).getBytes(StandardCharsets.UTF_8)
                    );
                f.dependencies().append(
                    "org.eolang",
                    "eo-runtime",
                    System.getProperty("eo.version", Manifests.read("EO-Version"))
                );
                new EoMavenPlugin(f)
                    .appended()
                    .execution("compile")
                    .goals("register", "compile", "transpile")
                    .configuration()
                    .set("failOnWarning", Boolean.FALSE.toString())
                    .set("skipLinting", Boolean.TRUE.toString());
                f.exec("clean", "compile");
            }
        );
        final Path generated = temp.resolve("target/generated-test-sources");
        if (Files.exists(generated)) {
            final List<String> names;
            try (Stream<Path> walk = Files.walk(generated)) {
                names = walk
                    .filter(Files::isRegularFile)
                    .map(p -> p.getFileName().toString())
                    .collect(Collectors.toList());
            }
            MatcherAssert.assertThat(
                "EO runtime test sources must not be generated in a user project, but these were found",
                names,
                Matchers.not(Matchers.hasItem(Matchers.containsString("EOstringTest")))
            );
        }
    }
}
