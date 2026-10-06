/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.manifests.Manifests;
import com.yegor256.Jaxec;
import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.farea.Farea;
import com.yegor256.farea.RequisiteMatcher;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration coverage for generated literal dataization.
 *
 * @since 0.1
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@ExtendWith({MktmpResolver.class, MayBeSlow.class})
final class LiteralDataizationIT {

    @Test
    void compilesAndRunsGeneratedLiteralJava(@Mktmp final Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                LiteralDataizationIT.compile(f);
                MatcherAssert.assertThat(
                    "the generated project must compile",
                    f.log(),
                    new RequisiteMatcher()
                        .with("BUILD SUCCESS")
                        .without("BUILD FAILURE")
                        .without("[ERROR]")
                );
            }
        );
        MatcherAssert.assertThat(
            "generated literals must dataize through the runtime",
            new Jaxec(
                "java", "-cp", String.format(
                    "%s%s%s", temp.resolve("target/classes"), File.pathSeparator,
                    Files.readString(temp.resolve("target/cp.txt"), StandardCharsets.UTF_8).trim()
                ), "-Dfile.encoding=UTF-8", "org.eolang.maven.LiteralSmoke"
            ).withHome(temp.resolve("target")).exec().stdout(),
            Matchers.containsString("literal-dataization-ok")
        );
    }

    private static void compile(final Farea farea) throws IOException {
        farea.properties()
            .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
            .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
        farea.files().file("src/main/eo/foo/x/main.eo").write(
            LiteralDataizationIT.program().getBytes(StandardCharsets.UTF_8)
        );
        farea.files().file("src/main/java/org/eolang/maven/LiteralSmoke.java").write(
            LiteralDataizationIT.smoke().getBytes(StandardCharsets.UTF_8)
        );
        farea.dependencies().append(
            "org.eolang", "eo-runtime",
            System.getProperty("eo.version", Manifests.read("EO-Version"))
        );
        new AppendedPlugin(farea).value()
            .phase("generate-sources")
            .goals("register", "parse", "transpile")
            .configuration()
            .set("offline", "true")
            .set("failOnWarning", "false")
            .set("skipLinting", "true");
        farea.build().plugins()
            .append("org.apache.maven.plugins", "maven-dependency-plugin", "3.7.0")
            .execution("classpath")
            .phase("compile")
            .goals("build-classpath")
            .configuration()
            .set("outputFile", "target/cp.txt");
        farea.exec("clean", "compile");
    }

    private static String program() {
        return String.join(
            System.lineSeparator(),
            "+architect yegor256@gmail.com",
            "+package foo.x",
            "",
            "[] > main",
            "  42 > number",
            "  \"雪だるま\" > string",
            "  01-AF > bytes"
        );
    }

    private static String smoke() {
        return String.join(
            System.lineSeparator(),
            "package org.eolang.maven;",
            "",
            "import java.nio.charset.StandardCharsets;",
            "import java.util.Arrays;",
            "import org.eolang.Dataized;",
            "import org.eolang.Phi;",
            "",
            "public final class LiteralSmoke {",
            "  public static void main(final String[] args) {",
            "    final Phi main = new org.eolang.EO_foo.EO_x.EOmain();",
            "    LiteralSmoke.check(new Dataized(main.take(\"number\")).take(), new byte[] {0x40, 0x45, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00});",
            "    LiteralSmoke.check(new Dataized(main.take(\"string\")).take(), \"雪だるま\".getBytes(StandardCharsets.UTF_8));",
            "    LiteralSmoke.check(new Dataized(main.take(\"bytes\")).take(), new byte[] {1, (byte) 0xAF});",
            "    final Phi number = main.take(\"number\");",
            "    LiteralSmoke.check(new Dataized(number.copy()).take(), new Dataized(number).take());",
            "    System.out.print(\"literal-dataization-ok\");",
            "  }",
            "  private static void check(final byte[] actual, final byte[] expected) {",
            "    if (!Arrays.equals(actual, expected)) {",
            "      throw new IllegalStateException(Arrays.toString(actual));",
            "    }",
            "  }",
            "}"
        );
    }
}
