/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
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
@SuppressWarnings({
    "JTCOP.RuleAllTestsHaveProductionClass",
    "PMD.UnitTestShouldIncludeAssert",
    "PMD.UnnecessaryVarargsArrayCreation"
})
@ExtendWith(MktmpResolver.class)
final class JarIT {

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void runsProgramFromJar(final @Mktmp Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                MatcherAssert.assertThat(
                    "simple program must be successfully executed",
                    new Jaxec(
                        "java", "-cp",
                        JarIT.compile(
                            f,
                            "# No comments.",
                            "[] > simple",
                            "  QQ.io.stdout > @",
                            "    \"Hello, world!\""
                        ),
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
    void runsProgramWithPackageFromJar(final @Mktmp Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                MatcherAssert.assertThat(
                    "'packaged' program must be successfully executed",
                    new Jaxec(
                        "java", "-cp",
                        JarIT.compile(
                            f,
                            "+package org.eolang.examples",
                            "",
                            "# Program with a package.",
                            "[args] > packaged",
                            "  QQ.io.stdout > @",
                            "    \"Hello, world from a program with a package!\""
                        ),
                        "-Dfile.encoding=UTF-8", "-Xss64M", "-Xms64M",
                        "org.eolang.Main", "org.eolang.examples.packaged"
                    ).withHome(temp.resolve("target")).exec().stdout(),
                    Matchers.containsString("Hello, world from a program with a package!")
                );
            }
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void runsProgramWithTwoObjects(final @Mktmp Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                MatcherAssert.assertThat(
                    "'fibonacci' program must be successfully executed",
                    new Jaxec(
                        "java", "-cp",
                        JarIT.compile(
                            f,
                            new ElegantObject(
                                "app",
                                new String[]{
                                    "+package org.eolang.examples",
                                    "+alias org.eolang.examples.fibonacci",
                                    "+alias org.eolang.io.stdout",
                                    "+alias org.eolang.tt.sprintf",
                                    "+alias org.eolang.tt.sscanf",
                                    "+architect yegor256@gmail.com",
                                    "",
                                    "# Application.",
                                    "[args] > app",
                                    "  number > n",
                                    "    at. > nn!",
                                    "      QQ.tt.sscanf",
                                    "        \"%d\"",
                                    "        args.at 0",
                                    "      0",
                                    "  at. > e!",
                                    "    QQ.tt.sscanf",
                                    "      \"%d\"",
                                    "      args.at 1",
                                    "    0",
                                    "  fibonacci n > f!",
                                    "  and. > @",
                                    "    stdout",
                                    "      sprintf",
                                    "        \"%dth Fibonacci number is %d\\n\"",
                                    "        * n f",
                                    "    e.eq f",
                                }
                            ),
                            new ElegantObject(
                                "fibonacci",
                                new String[]{
                                    "+package org.eolang.examples",
                                    "+architect yegor256@gmail.com",
                                    "",
                                    "# This is the main abstract object that",
                                    "# represents n-th Fibonacci number",
                                    "[n] > fibonacci",
                                    "  if. > @",
                                    "    lt.",
                                    "      n",
                                    "      2",
                                    "    n",
                                    "    plus.",
                                    "      ^.fibonacci",
                                    "        n.minus 1",
                                    "      ^.fibonacci",
                                    "        n.minus 2",
                                }
                            )
                        ),
                        "-Dfile.encoding=UTF-8", "-Xss64M", "-Xms64M",
                        "org.eolang.Main", "org.eolang.examples.app", "6", "8"
                    ).withHome(temp.resolve("target")).exec().stdout(),
                    Matchers.containsString("6th Fibonacci number is 8")
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
                MatcherAssert.assertThat(
                    "the program must throw an error and print it to stderr",
                    new Jaxec(
                        "java", "-cp",
                        JarIT.compile(
                            f,
                            "# No comments.",
                            "[] > simple",
                            "  unknown.io.stdout > @",
                            "    \"Hello, world!\""
                        ),
                        "-Dfile.encoding=UTF-8", "-Xss64M", "-Xms64M",
                        "org.eolang.Main", "simple"
                    ).withHome(temp.resolve("target")).withCheck(false).execUnsafe().stderr(),
                    Matchers.allOf(
                        Matchers.containsString("Couldn't find object 'Φ.org.eolang.unknown'"),
                        Matchers.containsString(
                            "because there's no class 'EOorg.EOeolang.EOunknown' or package-info class: 'EOorg.EOeolang.EOunknown.package-info"
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
        return JarIT.compile(farea, new ElegantObject(program));
    }

    /**
     * Compile EO program to XMIR and package it into a JAR.
     * @param farea Farea to use for compilation
     * @param objects The EO programs to compile
     * @return Classpath for the compiled program, with eo-runtime jar
     * @throws IOException If fails to compile
     */
    private static String compile(
        final Farea farea, final ElegantObject... objects
    ) throws IOException {
        farea.properties()
            .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
            .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
        for (final ElegantObject object : objects) {
            object.write(farea);
        }
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
        return String.join(
            File.pathSeparator,
            "test-0.0.0.jar",
            Paths.get(System.getProperty("user.home")).resolve(".m2")
                .resolve("repository")
                .resolve("org/eolang/eo-runtime")
                .resolve(ver)
                .resolve(String.format("eo-runtime-%s.jar", ver))
                .toString()
        );
    }

    /**
     * An EO object represented as a file name and its content.
     * @since 0.60
     */
    private static final class ElegantObject {
        /**
         * File name.
         */
        private final String file;

        /**
         * File content.
         */
        private final String content;

        /**
         * Ctor.
         * @param content File content
         */
        private ElegantObject(final String... content) {
            this("simple", content);
        }

        /**
         * Ctor.
         * @param file File name
         * @param content File content
         */
        private ElegantObject(final String file, final String... content) {
            this.file = file;
            this.content = String.join("\n", content);
        }

        /**
         * Write the EO object to Farea.
         * @param farea Farea
         * @throws IOException If fails to write
         */
        void write(final Farea farea) throws IOException {
            farea.files()
                .file(String.format("src/main/eo/%s.eo", this.file))
                .write(this.content.getBytes(StandardCharsets.UTF_8));
        }
    }
}
