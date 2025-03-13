/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2025 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package integration;

import com.jcabi.manifests.Manifests;
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
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * Integration test for phi-unphi.
 *
 * @since 0.1
 * @todo #3199:30min Enable PhiUnphiIT. The test was disabled because tuples are converted to EO
 *  incorrectly after phi-unphi. The key problem is recursive representation of tuples via
 *  tuple.with method. It should be done via simple application of tuple like it was done before,
 *  but with calculated length.
 *  Check the disabled org/eolang/parser/eo-packs/print/tuples-of-tuples-to-stars.yaml pack in
 *  eo-parser module.
 * @checkstyle MethodLengthCheck (500 lines)
 */
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith(MktmpResolver.class)
final class PhiUnphiIT {

    @Test
    @ExtendWith(MayBeSlow.class)
    @ExtendWith(WeAreOnline.class)
    void runsTestsAfterPhiAndUnphi(final @Mktmp Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                f.files().file("src/main").save(
                    Paths.get(System.getProperty("user.dir")).resolve("src/main")
                );
                f.files().file("src/test/eo").save(
                    Paths.get(System.getProperty("user.dir")).resolve("src/test/eo")
                );
                f.properties()
                    .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
                    .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
                f.dependencies().append(
                    "net.sf.saxon",
                    "Saxon-HE",
                    "12.4"
                );
                f.build()
                    .plugins()
                    .append(
                        "org.eolang",
                        "eo-maven-plugin",
                        System.getProperty(
                            "eo.version",
                            Manifests.read("EO-Version")
                        )
                    )
                    .execution("phi-unphi")
                    .phase("process-sources")
                    .goals(
                        "register",
                        "parse",
                        "xmir-to-phi",
                        "phi-to-xmir",
                        "print"
                    )
                    .configuration()
                    .set("sourcesDir", "${project.basedir}/src/test/eo")
                    .set("targetDir", "${project.build.directory}/eo-test")
                    .set("phiInputDir", "${project.build.directory}/eo-test/1-parse")
                    .set("phiOutputDir", "${project.basedir}/src/phi")
                    .set("unphiInputDir", "${project.basedir}/src/phi")
                    .set("unphiOutputDir", "${project.basedir}/src/unphi")
                    .set("unphiMetas", new String[]{"+tests", "+unlint decorated-formation"})
                    .set("printSourcesDir", "${project.basedir}/src/unphi")
                    .set("printOutputDir", "${project.basedir}/src/test/generated-eo");
                f.exec("clean", "process-sources");
                MatcherAssert.assertThat(
                    "Converting to phi and back was not successful",
                    f.log(),
                    RequisiteMatcher.SUCCESS
                );
                f.files().file("pom.xml").delete();
                f.properties()
                    .set("project.build.sourceEncoding", StandardCharsets.UTF_8.name())
                    .set("project.reporting.outputEncoding", StandardCharsets.UTF_8.name());
                f.dependencies().append(
                    "org.junit.jupiter",
                    "junit-jupiter-engine",
                    "5.10.3"
                );
                f.dependencies().append(
                    "org.junit.jupiter",
                    "junit-jupiter-params",
                    "5.10.3"
                );
                f.dependencies().append(
                    "org.junit.jupiter",
                    "junit-jupiter-api",
                    "5.10.3"
                );
                f.dependencies().append(
                    "org.junit-pioneer",
                    "junit-pioneer",
                    "2.2.0"
                );
                f.build()
                    .plugins()
                    .append(
                        "org.eolang",
                        "eo-maven-plugin",
                        System.getProperty(
                            "eo.version",
                            Manifests.read("EO-Version")
                        )
                    )
                    .execution("compile")
                    .goals(
                        "register",
                        "compile",
                        "transpile",
                        "copy",
                        "unplace",
                        "unspile"
                    )
                    .configuration()
                    .set("foreign", "${project.basedir}/target/eo-foreign.json")
                    .set("foreignFormat", "csv")
                    .set("failOnWarning", Boolean.FALSE.toString())
                    .set("offline", Boolean.TRUE.toString())
                    .set("skipLinting", Boolean.TRUE.toString())
                    .set("withRuntimeDependency", Boolean.FALSE.toString())
                    .set("placeBinariesThatHaveSources", Boolean.TRUE.toString());
                f.build()
                    .plugins()
                    .append(
                        "org.eolang",
                        "eo-maven-plugin",
                        System.getProperty(
                            "eo.version",
                            "1.0-SNAPSHOT"
                        )
                    )
                    .execution("tests")
                    .phase("generate-test-sources")
                    .goals(
                        "register",
                        "compile",
                        "transpile"
                    )
                    .configuration()
                    .set("foreign", "${project.basedir}/target/eo-foreign.json")
                    .set("foreignFormat", "csv")
                    .set("failOnWarning", Boolean.FALSE.toString())
                    .set("offline", Boolean.TRUE.toString())
                    .set("scope", "test")
                    .set("sourcesDir", "${project.basedir}/src/test/generated-eo")
                    .set("targetDir", "${project.basedir}/target/eo-test")
                    .set("addSourcesRoot", Boolean.FALSE.toString())
                    .set("addTestSourcesRoot", Boolean.TRUE.toString())
                    .set("failOnWarning", Boolean.FALSE.toString())
                    .set("generatedDir", "${project.basedir}/target/generated-test-sources")
                    .set("withRuntimeDependency", Boolean.FALSE.toString())
                    .set("placeBinariesThatHaveSources", Boolean.TRUE.toString());
                f.exec("clean", "test");
                MatcherAssert.assertThat(
                    "Some tests weren't passed after converting to phi and back",
                    f.log(),
                    RequisiteMatcher.SUCCESS
                );
            }
        );
    }
}
