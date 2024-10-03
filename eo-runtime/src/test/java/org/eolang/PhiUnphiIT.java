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
package org.eolang;

import com.yegor256.WeAreOnline;
import com.yegor256.farea.Farea;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.stream.Collectors;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Integration test for phi-unphi.
 *
 * @since 0.1
 */
@ExtendWith(WeAreOnline.class)
@SuppressWarnings({"JTCOP.RuleAllTestsHaveProductionClass", "JTCOP.RuleNotContainsTestWord"})
final class PhiUnphiIT {

    /**
     * True.
     */
    private static final String TRUE = "true";

    /**
     * False.
     */
    private static final String FALSE = "false";

    /**
     * UTF-8.
     */
    private static final String UTF_8 = "UTF-8";

    /**
     * The eo.version.
     */
    private static final String EO_VERSION = "eo.version";

    /**
     * The eo-maven-plugin.
     */
    private static final String EO_PLUGIN = "eo-maven-plugin";

    /**
     * The org.eolang.
     */
    private static final String EO_GROUP = "org.eolang";

    /**
     * The 1.0-SNAPSHOT.
     */
    private static final String SNAPSHOT_1_0 = "1.0-SNAPSHOT";

    // @checkstyle MethodLengthCheck (170 lines)
    @Test
    @ExtendWith(WeAreOnline.class)
    void runsTestsAfterPhiAndUnphi(final @TempDir Path temp) throws IOException {
        new Farea(temp).together(
            f -> {
                PhiUnphiIT.copySources(f, "src/main");
                PhiUnphiIT.copySources(f, "src/test/eo");
                f.properties()
                    .set("project.build.sourceEncoding", PhiUnphiIT.UTF_8)
                    .set("project.reporting.outputEncoding", PhiUnphiIT.UTF_8);
                f.dependencies().append(
                    "net.sf.saxon",
                    "Saxon-HE",
                    "12.4"
                );
                f.build()
                    .plugins()
                    .append(
                        PhiUnphiIT.EO_GROUP,
                        PhiUnphiIT.EO_PLUGIN,
                        System.getProperty(
                            PhiUnphiIT.EO_VERSION,
                            PhiUnphiIT.SNAPSHOT_1_0
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
                    .set("phiOutputDir", "${project.build.directory}/phi")
                    .set("unphiInputDir", "${project.build.directory}/phi")
                    .set("unphiOutputDir", "${project.build.directory}/generated-eo-test/1-parse")
                    .set("unphiMetas", new String[]{"+tests"})
                    .set("printSourcesDir", "${project.build.directory}/generated-eo-test/1-parse")
                    .set("printOutputDir", "${project.basedir}/src/test/generated-eo")
                    .set("printReversed", PhiUnphiIT.TRUE);
                f.exec("clean", "compile");
                final String phi = f.log();
                MatcherAssert.assertThat(
                    "Converting to phi and back was not successful",
                    phi,
                    Matchers.containsString("BUILD SUCCESS")
                );
                f.files().file("pom.xml").delete();
                f.properties()
                    .set("project.build.sourceEncoding", PhiUnphiIT.UTF_8)
                    .set("project.reporting.outputEncoding", PhiUnphiIT.UTF_8);
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
                        PhiUnphiIT.EO_GROUP,
                        PhiUnphiIT.EO_PLUGIN,
                        System.getProperty(
                            PhiUnphiIT.EO_VERSION,
                            PhiUnphiIT.SNAPSHOT_1_0
                        )
                    )
                    .execution("compile")
                    .goals(
                        "register",
                        "assemble",
                        "verify",
                        "transpile",
                        "copy",
                        "unplace",
                        "unspile"
                    )
                    .configuration()
                    .set("foreign", "${project.basedir}/target/eo-foreign.csv")
                    .set("foreignFormat", "csv")
                    .set("failOnWarning", PhiUnphiIT.FALSE)
                    .set("offline", PhiUnphiIT.TRUE)
                    .set("withRuntimeDependency", PhiUnphiIT.FALSE)
                    .set("placeBinariesThatHaveSources", PhiUnphiIT.TRUE);
                f.build()
                    .plugins()
                    .append(
                        PhiUnphiIT.EO_GROUP,
                        PhiUnphiIT.EO_PLUGIN,
                        System.getProperty(
                            PhiUnphiIT.EO_VERSION,
                            PhiUnphiIT.SNAPSHOT_1_0
                        )
                    )
                    .execution("deps")
                    .phase("process-sources")
                    .goals("deps");
                f.build()
                    .plugins()
                    .append(
                        PhiUnphiIT.EO_GROUP,
                        PhiUnphiIT.EO_PLUGIN,
                        System.getProperty(
                            PhiUnphiIT.EO_VERSION,
                            PhiUnphiIT.SNAPSHOT_1_0
                        )
                    )
                    .execution("tests")
                    .phase("generate-test-sources")
                    .goals(
                        "register",
                        "assemble",
                        "verify",
                        "transpile",
                        "binarize"
                    )
                    .configuration()
                    .set("foreign", "${project.basedir}/target/eo-foreign.csv")
                    .set("foreignFormat", "csv")
                    .set("failOnWarning", PhiUnphiIT.FALSE)
                    .set("offline", PhiUnphiIT.TRUE)
                    .set("scope", "test")
                    .set("sourcesDir", "${project.basedir}/src/test/generated-eo")
                    .set("targetDir", "${project.basedir}/target/eo-test")
                    .set("addSourcesRoot", PhiUnphiIT.FALSE)
                    .set("addTestSourcesRoot", PhiUnphiIT.TRUE)
                    .set("failOnWarning", PhiUnphiIT.FALSE)
                    .set("generatedDir", "${project.basedir}/target/generated-test-sources")
                    .set("withRuntimeDependency", PhiUnphiIT.FALSE)
                    .set("placeBinariesThatHaveSources", PhiUnphiIT.TRUE);
                f.exec("clean", "test");
                MatcherAssert.assertThat(
                    "Some tests weren't passed after converting to phi and back",
                    f.log(),
                    Matchers.containsString("BUILD SUCCESS")
                );
            }
        );
    }

    /**
     * Copy EO sources.
     * @param farea Farea instance
     * @param target Directory to copy from
     * @throws IOException If fails to copy files
     */
    private static void copySources(final Farea farea, final String target) throws IOException {
        final Path runtime = Paths.get(System.getProperty("user.dir"))
            .resolve(target);
        final Collection<Path> sources = Files.walk(runtime)
            .filter(src -> !src.toFile().isDirectory())
            .collect(Collectors.toList());
        for (final Path src : sources) {
            farea.files()
                .file(String.format("%s/%s", target, runtime.relativize(src)))
                .write(new UncheckedText(new TextOf(src)).asString())
                .show();
        }
    }
}
