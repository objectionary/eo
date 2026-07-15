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
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test that runs simple EO program from packaged jar.
 * @since 0.54
 * @todo #4750:30min Re-enable JarIT after next release.
 *  The suite is temporarily disabled because its sandbox transpiles
 *  eo-runtime objects pulled from the remote objectionary, where
 *  number.floor is still defined as a Java atom. That transpilation
 *  emits a reference to EOnumber$EOfloor, which has been removed from
 *  the runtime in this branch, so javac fails. Once a release is cut
 *  and the remote catches up with the EO-level floor, drop this
 *  annotation.
 */
@SuppressWarnings("JTCOP.RuleAllTestsHaveProductionClass")
@ExtendWith(MktmpResolver.class)
@Disabled
final class JarIT {

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void runsProgramFromJar(final @Mktmp Path temp) throws IOException {
        final String[] classpath = {""};
        new Farea(temp).together(
            f -> classpath[0] = JarIT.compile(
                f,
                "[] > simple",
                "  Q.io.stdout > @",
                "    \"Hello, world!\""
            )
        );
        MatcherAssert.assertThat(
            "simple program must be successfully executed",
            new Jaxec(
                "java", "-cp", classpath[0],
                "-Dfile.encoding=UTF-8", "-Xss64M", "-Xms64M",
                "org.eolang.Main", "simple"
            ).withHome(temp.resolve("target")).exec().stdout(),
            Matchers.containsString("Hello, world!")
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void runsProgramWithPackageFromJar(final @Mktmp Path temp) throws IOException {
        final String[] classpath = {""};
        new Farea(temp).together(
            f -> classpath[0] = JarIT.compile(
                f,
                "+package examples",
                "",
                "[args] > packaged",
                "  Q.io.stdout > @",
                "    \"Hello, world from a program with a package!\""
            )
        );
        MatcherAssert.assertThat(
            "'packaged' program must be successfully executed",
            new Jaxec(
                "java", "-cp", classpath[0],
                "-Dfile.encoding=UTF-8", "-Xss64M", "-Xms64M",
                "org.eolang.Main", "examples.packaged"
            ).withHome(temp.resolve("target")).exec().stdout(),
            Matchers.containsString("Hello, world from a program with a package!")
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void runsProgramWithTwoObjects(final @Mktmp Path temp) throws IOException {
        final String[] classpath = {""};
        new Farea(temp).together(
            f -> classpath[0] = JarIT.compile(
                f,
                JarIT.ElegantObject.made("app", JarIT.appProgram()),
                JarIT.ElegantObject.made("fibonacci", JarIT.fibonacciProgram())
            )
        );
        MatcherAssert.assertThat(
            "'fibonacci' program must be successfully executed",
            new Jaxec(
                "java", "-cp", classpath[0],
                "-Dfile.encoding=UTF-8", "-Xss64M", "-Xms64M",
                "org.eolang.Main", "examples.app", "6", "8"
            ).withHome(temp.resolve("target")).exec().stdout(),
            Matchers.containsString("6th Fibonacci number is 8")
        );
    }

    @Test
    @ExtendWith(WeAreOnline.class)
    @ExtendWith(MayBeSlow.class)
    void printsErrorToStderr(final @Mktmp Path temp) throws IOException {
        final String[] classpath = {""};
        new Farea(temp).together(
            f -> classpath[0] = JarIT.compile(
                f,
                "[] > simple",
                "  unknown.io.stdout > @",
                "    \"Hello, world!\""
            )
        );
        MatcherAssert.assertThat(
            "the program must throw an error and print it to stderr",
            new Jaxec(
                "java", "-cp", classpath[0],
                "-Dfile.encoding=UTF-8", "-Xss64M", "-Xms64M",
                "org.eolang.Main", "simple"
            ).withHome(temp.resolve("target")).withCheck(false).execUnsafe().stderr(),
            Matchers.allOf(
                Matchers.containsString("Couldn't find object '\\u03a6.unknown'"),
                Matchers.containsString(
                    "because there's no class 'org.eolang.EOunknown' or package-info class: 'org.eolang.EOunknown.package-info'"
                )
            )
        );
    }

    private static String[] appProgram() {
        return new String[]{
            "+package examples",
            "+alias examples.fibonacci",
            "+alias io.stdout",
            "+alias tt.sprintf",
            "+alias tt.sscanf",
            "+architect yegor256@gmail.com",
            "",
            "[args] > app",
            "  number > n",
            "    at. > nn!",
            "      Q.tt.sscanf",
            "        \"%d\"",
            "        args.at 0",
            "      0",
            "  at. > e!",
            "    Q.tt.sscanf",
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
        };
    }

    private static String[] fibonacciProgram() {
        return new String[]{
            "+package examples",
            "+architect yegor256@gmail.com",
            "",
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
        };
    }

    /**
     * Compile EO program to XMIR and package it into a JAR.
     * @param farea Farea to use for compilation
     * @param program The EO program to compile
     * @return Classpath for the compiled program, with eo-runtime jar
     * @throws IOException If fails to compile
     */
    private static String compile(final Farea farea, final String... program) throws IOException {
        return JarIT.compile(farea, JarIT.ElegantObject.made(program));
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
        farea.dependencies().append(
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
            new RequisiteMatcher()
                .with("BUILD SUCCESS")
                .without("BUILD FAILURE")
                .without("[ERROR]")
        );
        return String.join(
            File.pathSeparator,
            "test-0.0.0.jar",
            Paths.get(System.getProperty("user.home")).resolve(".m2")
            .resolve("repository")
            .resolve("org/eolang/eo-runtime").resolve(
                System.getProperty("eo.version", Manifests.read("EO-Version"))
            ).resolve(
                String.format(
                    "eo-runtime-%s.jar",
                    System.getProperty("eo.version", Manifests.read("EO-Version"))
                )
            )
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
         * @param file File name
         * @param content Joined file content
         */
        private ElegantObject(final String file, final String content) {
            this.file = file;
            this.content = content;
        }

        /**
         * Factory.
         * @param content File content lines
         * @return New ElegantObject
         */
        static JarIT.ElegantObject made(final String... content) {
            return JarIT.ElegantObject.made("simple", content);
        }

        /**
         * Factory.
         * @param file File name
         * @param content File content lines
         * @return New ElegantObject
         */
        static JarIT.ElegantObject made(final String file, final String... content) {
            return new JarIT.ElegantObject(file, String.join(System.lineSeparator(), content));
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
