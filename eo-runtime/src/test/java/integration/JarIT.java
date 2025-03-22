/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.yegor256.Jaxec;
import com.yegor256.MayBeSlow;
import com.yegor256.Mktmp;
import com.yegor256.MktmpResolver;
import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import com.yegor256.farea.RequisiteMatcher;
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
                f.properties()
                    .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
                    .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
                f.files().file("src/main").save(
                    Paths.get(System.getProperty("user.dir")).resolve("src/main")
                );
                f.files()
                    .file("src/main/eo/simple.eo")
                    .write(
                        "QQ.io.stdout \"Hello, world!\" > simple\n".getBytes(StandardCharsets.UTF_8)
                    );
                new EoMavenPlugin(f)
                    .appended()
                    .execution("compile")
                    .goals("register", "compile", "transpile")
                    .configuration()
                    .set("ignoreRuntime", Boolean.TRUE.toString())
                    .set("offline", Boolean.TRUE.toString())
                    .set("failOnWarning", Boolean.FALSE.toString())
                    .set("skipLinting", Boolean.TRUE.toString());
                f.exec("clean", "compile", "jar:jar");
                MatcherAssert.assertThat(
                    "Project must be successfully built and packaged into jar",
                    f.log(),
                    RequisiteMatcher.SUCCESS
                );
                MatcherAssert.assertThat(
                    "Simple program must be successfully executed from jar",
                    new Jaxec(
                        "java", "-cp", "test-0.0.0.jar",
                        "-Dfile.encoding=UTF-8", "-Xss64M", "-Xms64M",
                        "org.eolang.Main", "simple"
                    ).withHome(temp.resolve("target")).exec().stdout(),
                    Matchers.allOf(
                        Matchers.containsString("Hello, world!"),
                        Matchers.containsString("[0x01] = true")
                    )
                );
            }
        );
    }
}
