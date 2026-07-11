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
import java.util.Collections;
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
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith({WeAreOnline.class, MktmpResolver.class, MayBeSlow.class})
final class TranspileIT {

    @Test
    void generatesTestSourcesForUserWrittenTests(final @Mktmp Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                TranspileIT.setup(f, TranspileIT.programWithTests());
                MatcherAssert.assertThat(
                    "User-written test sources must be generated",
                    TranspileIT.generatedNames(temp),
                    Matchers.hasItem("EOsimpleTest.java")
                );
            }
        );
    }

    @Test
    void doesNotGenerateRuntimeTestSourcesForUserProject(
        final @Mktmp Path temp
    ) throws IOException {
        new Farea(temp).together(
            f -> {
                TranspileIT.setup(f, TranspileIT.simpleProgram());
                MatcherAssert.assertThat(
                    "EO runtime test sources must not be generated in a user project",
                    TranspileIT.generatedNames(temp),
                    Matchers.not(Matchers.hasItem(Matchers.containsString("EOstringTest")))
                );
            }
        );
    }

    private static void setup(final Farea farea, final byte[] program) throws IOException {
        farea.properties()
            .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
            .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
        farea.files().file("src/main/eo/simple.eo").write(program);
        farea.dependencies().append(
            "org.eolang", "eo-runtime",
            System.getProperty("eo.version", Manifests.read("EO-Version"))
        );
        new EoMavenPlugin(farea).appended()
            .execution("transpile-it")
            .goals("register", "compile", "transpile")
            .configuration()
            .set("failOnWarning", Boolean.FALSE.toString())
            .set("skipLinting", Boolean.TRUE.toString());
        farea.exec("clean", "compile");
    }

    private static List<String> generatedNames(final Path temp) throws IOException {
        final Path dir = temp.resolve("target/generated-test-sources");
        final List<String> result;
        if (Files.exists(dir)) {
            try (Stream<Path> walk = Files.walk(dir)) {
                result = walk.filter(Files::isRegularFile)
                    .map(p -> p.getFileName().toString())
                    .collect(Collectors.toList());
            }
        } else {
            result = Collections.emptyList();
        }
        return result;
    }

    private static byte[] simpleProgram() {
        return String.join(
            System.lineSeparator(),
            "[] > simple",
            "  \"hello\" > @"
        ).getBytes(StandardCharsets.UTF_8);
    }

    private static byte[] programWithTests() {
        return String.join(
            System.lineSeparator(),
            "[] > simple",
            "  \"hello\" > @",
            "  [] +> tests-simple-works",
            "    true > @"
        ).getBytes(StandardCharsets.UTF_8);
    }
}
