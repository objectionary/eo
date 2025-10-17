/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.jcabi.manifests.Manifests;
import com.yegor256.Jaxec;
import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import com.yegor256.farea.RequisiteMatcher;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test that runs simple EO program from packaged jar.
 * @since 0.54
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@ExtendWith(MktmpResolver.class)
final class JarIT {

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void runsProgramFromJar(final @Mktmp Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                final String classpath = JarIT.compile(
                    f,
                    "# No comments.",
                    "[] > simple",
                    "  QQ.io.stdout > @",
                    "    \"Hello, world!\""
                );
                MatcherAssert.assertThat(
                    "simple program must be successfully executed from jar",
                    new Jaxec(
                        "java", "-cp", classpath,
                        "-Dfile.encoding=UTF-8", "-Xss64M", "-Xms64M",
                        "org.eolang.Main", "simple"
                    ).withHome(temp.resolve("target")).exec().stdout(),
                    Matchers.containsString("Hello, world!")
                );
            }
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void printsErrorToStderr(final @Mktmp Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                final String classpath = JarIT.compile(
                    f,
                    "# No comments.",
                    "[] > simple",
                    "  unknown.io.stdout > @",
                    "    \"Hello, world!\""
                );
                MatcherAssert.assertThat(
                    "the program must throw an error and print it to stderr",
                    new Jaxec(
                        "java", "-cp", classpath,
                        "-Dfile.encoding=UTF-8", "-Xss64M", "-Xms64M",
                        "org.eolang.Main", "simple"
                    ).withHome(temp.resolve("target")).withCheck(false).execUnsafe().stderr(),
                    Matchers.allOf(
                        Matchers.containsString("Couldn't find object 'Φ.org.eolang.unknown'"),
                        Matchers.containsString(
                            "because there's no class or package 'EOorg.EOeolang.EOunknown"
                        )
                    )
                );
            }
        );
    }

    /**
     * Compile EO program to XMIR and package it into a JAR.
     * @param farea Farea to use for compilation
     * @param program The EO program to compile
     * @return Classpath for the compiled program, with eo-runtime jar
     * @throws IOException If fails to compile
     */
    private static String compile(final Farea farea, final String... program) throws IOException {
        farea.properties()
            .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
            .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
        farea.files()
            .file("src/main/eo/simple.eo")
            .write(String.join("\n", program).getBytes(StandardCharsets.UTF_8));
        farea.dependencies()
            .append(
                "org.eolang",
                "eo-runtime",
                System.getProperty(
                    "eo.version",
                    Manifests.read("EO-Version")
                )
            );
        new EoMavenPlugin(farea)
            .appended()
            .execution("compile")
            .goals("register", "compile", "transpile")
            .configuration()
            .set("ignoreRuntime", Boolean.TRUE.toString())
            .set("failOnWarning", Boolean.FALSE.toString())
            .set("skipLinting", Boolean.TRUE.toString());
        farea.exec("clean", "compile", "jar:jar");
        MatcherAssert.assertThat(
            "Project must be successfully built and packaged into jar",
            farea.log(),
            RequisiteMatcher.SUCCESS
        );
        final String ver = System.getProperty("eo.version", Manifests.read("EO-Version"));
        final String jar = String.format("eo-runtime-%s.jar", ver);
        final String runtime = Paths.get(System.getProperty("user.home")).resolve(".m2")
            .resolve("repository")
            .resolve("org/eolang/eo-runtime")
            .resolve(ver)
            .resolve(jar)
            .toString();
        return String.join(
            File.pathSeparator,
            "test-0.0.0.jar",
            runtime
        );
    }
}
