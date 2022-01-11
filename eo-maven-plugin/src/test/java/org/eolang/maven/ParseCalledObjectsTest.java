/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Csv;
import com.yegor256.tojos.MonoTojos;
import java.nio.file.Path;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.hamcrest.MatcherAssert;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.xmlunit.matchers.CompareMatcher;

/**
 * Test case for called-objects.xsl transformation.
 *
 * @since 0.21
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class ParseCalledObjectsTest {

    @Test
    void callsLocalObject(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("sandbox/app.eo");
        new Save(
            String.join(
                System.lineSeparator(),
                "+package sandbox",
                "+alias org.eolang.io.stdout",
                "+alias org.eolang.txt.sprintf",
                "",
                "[] > app",
                "  1 > c",
                "  stdout > @",
                "    sprintf",
                "      \"Your number is %s\"",
                "      (f 1 2 3 4)",
                "",
                "[n args...] > f",
                "  1 > a",
                "  args.length > @"
            ),
            src
        ).save();
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        final Path registry = temp.resolve("eo-header-registry.xml");
        final Path generated = temp.resolve("generated");
        new MonoTojos(new Csv(foreign))
            .add("sandbox.app")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("headerRegistry", registry.toFile())
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("headerRegistry", registry.toFile())
            .execute();
        new Moja<>(TranspileMojo.class)
            .with("compiler", "canonical")
            .with("project", new MavenProjectStub())
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("headerRegistry", registry.toFile())
            .execute();
        final XML result = new XMLDocument(
            target.resolve(
                String.format("%s/sandbox/app/00-pre-called-objects.xml", TranspileMojo.PRE)
            )
        );
        MatcherAssert.assertThat(
            result.nodes("//o[@base=\"f\"]").get(0).toString(),
            CompareMatcher.isIdenticalTo(
                String.join(
                    System.lineSeparator(),
                    "<o base=\"f\" line=\"10\" ref=\"12\">",
                    "   <o base=\"org.eolang.int\" data=\"int\" line=\"10\">1</o>",
                    "   <o base=\"org.eolang.array\" data=\"array\" line=\"10\" unvaring=\"\">",
                    "       <o base=\"org.eolang.int\" data=\"int\" line=\"10\">2</o>",
                    "       <o base=\"org.eolang.int\" data=\"int\" line=\"10\">3</o>",
                    "       <o base=\"org.eolang.int\" data=\"int\" line=\"10\">4</o>",
                    "   </o>",
                    "</o>"
                )
            ).ignoreWhitespace()
        );
    }

    @Test
    void callsExternalObject(@TempDir final Path temp) throws Exception {
        final Path smallsrc = temp.resolve("sandbox/small.eo");
        new Save(
            String.join(
                System.lineSeparator(),
                "+package sandbox",
                "",
                "[n args...] > small",
                "  if. > @",
                "    (args.get 0).eq 2",
                "    1",
                "    n"
            ),
            smallsrc
        ).save();
        final Path appsrc = temp.resolve("sandbox/app.eo");
        new Save(
            String.join(
                System.lineSeparator(),
                "+package sandbox",
                "+alias sandbox.small",
                "+alias org.eolang.io.stdout",
                "+alias org.eolang.txt.sprintf",
                "",
                "[] > app",
                "  1 > c",
                "  stdout > @",
                "    sprintf",
                "      \"Your number is %s\"",
                "      (small 1 2 3)"
            ),
            appsrc
        ).save();
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        final Path registry = temp.resolve("eo-header-registry.xml");
        final Path generated = temp.resolve("generated");
        new MonoTojos(new Csv(foreign))
            .add("sandbox.small")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, smallsrc.toString());
        new MonoTojos(new Csv(foreign))
            .add("sandbox.app")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, appsrc.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("headerRegistry", registry.toFile())
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("headerRegistry", registry.toFile())
            .execute();
        new Moja<>(TranspileMojo.class)
            .with("compiler", "canonical")
            .with("project", new MavenProjectStub())
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("headerRegistry", registry.toFile())
            .execute();
        final XML result = new XMLDocument(
            target.resolve(
                String.format("%s/sandbox/app/00-pre-called-objects.xml", TranspileMojo.PRE)
            )
        );
        MatcherAssert.assertThat(
            result.nodes("//o[@base=\"sandbox.small\"]").get(0).toString(),
            CompareMatcher.isIdenticalTo(
                String.join(
                    "\n",
                    "<o base=\"sandbox.small\" line=\"11\">",
                    "   <o base=\"org.eolang.int\" data=\"int\" line=\"11\">1</o>",
                    "   <o base=\"org.eolang.array\" data=\"array\" line=\"11\" unvaring=\"\">",
                    "       <o base=\"org.eolang.int\" data=\"int\" line=\"11\">2</o>",
                    "       <o base=\"org.eolang.int\" data=\"int\" line=\"11\">3</o>",
                    "   </o>",
                    "</o>"
                )
            ).ignoreWhitespace()
        );
    }
}
