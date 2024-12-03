/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package integration;

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
 * Integration test for phi-unphi.
 *
 * @since 0.1
 */
@ExtendWith(WeAreOnline.class)
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
@ExtendWith(MktmpResolver.class)
final class PhiUnphiIT {

    // @checkstyle MethodLengthCheck (170 lines)
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
                            "1.0-SNAPSHOT"
                        )
                    )
                    .execution("phi-unphi")
                    .phase("process-sources")
                    .goals(
                        "register",
                        "deps",
                        "parse",
                        "optimize",
                        "xmir-to-phi",
                        "phi-to-xmir",
                        "print"
                    )
                    .configuration()
                    .set("sourcesDir", "${project.basedir}/src/test/eo")
                    .set("targetDir", "${project.build.directory}/eo-test")
                    .set("phiInputDir", "${project.build.directory}/eo-test/2-optimize")
                    .set("phiOutputDir", "${project.basedir}/src/phi")
                    .set("unphiInputDir", "${project.basedir}/src/phi")
                    .set("unphiOutputDir", "${project.build.directory}/generated-eo-test/1-parse")
                    .set("unphiMetas", new String[]{"+tests", "+unlint abstract-decoratee"})
                    .set("printSourcesDir", "${project.build.directory}/generated-eo-test/1-parse")
                    .set("printOutputDir", "${project.basedir}/src/test/generated-eo")
                    .set("printReversed", Boolean.TRUE.toString());
                f.exec("clean", "compile");
                final String phi = f.log().content();
                MatcherAssert.assertThat(
                    "Converting to phi and back was not successful",
                    phi,
                    Matchers.containsString("BUILD SUCCESS")
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
                            "1.0-SNAPSHOT"
                        )
                    )
                    .execution("compile")
                    .goals(
                        "register",
                        "assemble",
                        "lint",
                        "transpile",
                        "copy",
                        "unplace",
                        "unspile"
                    )
                    .configuration()
                    .set("foreign", "${project.basedir}/target/eo-foreign.csv")
                    .set("foreignFormat", "csv")
                    .set("failOnWarning", Boolean.FALSE.toString())
                    .set("offline", Boolean.TRUE.toString())
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
                    .execution("deps")
                    .phase("process-sources")
                    .goals("deps");
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
                        "assemble",
                        "lint",
                        "transpile",
                        "binarize"
                    )
                    .configuration()
                    .set("foreign", "${project.basedir}/target/eo-foreign.csv")
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
