/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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

import com.jcabi.matchers.XhtmlMatchers;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StXSL;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link OptimizeMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class OptimizeMojoTest {

    @Test
    void skipsAlreadyOptimized(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+package f",
                "[args] > main",
                "  (stdout \"Hello!\").print > @"
            )
            .execute(ParseMojo.class)
            .execute(OptimizeMojo.class);
        final Path path = maven.result().get(
            String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
        );
        final long mtime = path.toFile().lastModified();
        maven.execute(OptimizeMojo.class);
        MatcherAssert.assertThat(
            path.toFile().lastModified(),
            Matchers.is(mtime)
        );
    }

    @Test
    void optimizesIfExpired(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final Path tgt = maven
            .withProgram(
                "+package f",
                "[args] > main",
                "  (stdout \"Hello!\").print > @"
            )
            .execute(ParseMojo.class)
            .execute(OptimizeMojo.class)
            .result()
            .get(
                String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
            );
        final long start = System.currentTimeMillis();
        if (!tgt.toFile().setLastModified(start - TimeUnit.SECONDS.toMillis(10L))) {
            Assertions.fail(String.format("The last modified attribute can't be set for %s", tgt));
        }
        maven.execute(OptimizeMojo.class);
        MatcherAssert.assertThat(
            tgt.toFile().lastModified(),
            Matchers.greaterThan(start)
        );
    }

    /**
     * Test case for #1223.
     *
     * @param temp Temporary test directory.
     * @throws Exception if unexpected error happened.
     */
    @Test
    void getsAlreadyOptimizedResultsFromCache(@TempDir final Path temp) throws Exception {
        final TextOf cached = new TextOf(
            new ResourceOf("org/eolang/maven/optimize/main.xml")
        );
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new Home(cache).save(
            cached,
            Paths.get(OptimizeMojo.OPTIMIZED)
                .resolve(hash)
                .resolve("foo/x/main.xmir")
        );
        new FakeMaven(temp)
            .withProgram(
                "+package f",
                "[args] > main",
                "  (stdout \"Hello!\").print > @"
            )
            .with("cache", cache)
            .withTojoAttribute(AssembleMojo.ATTR_HASH, hash)
            .execute(ParseMojo.class)
            .execute(OptimizeMojo.class);
        MatcherAssert.assertThat(
            new XMLDocument(
                new Home(temp).load(
                    Paths.get(
                        String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR,
                            TranspileMojo.EXT
                        )
                    )
                ).asBytes()
            ),
            Matchers.is(new XMLDocument(cached.asString()))
        );
    }

    @Test
    void savesOptimizedResultsToCache(@TempDir final Path temp) throws Exception {
        final Path cache = temp.resolve("cache");
        final String hash = "abcdef1";
        new FakeMaven(temp)
            .withProgram(
                "+package f",
                "[args] > main",
                "  (stdout \"Hello!\").print > @"
            )
            .with("cache", cache)
            .withTojoAttribute(AssembleMojo.ATTR_HASH, hash)
            .execute(ParseMojo.class)
            .execute(OptimizeMojo.class);
        MatcherAssert.assertThat(
            cache.resolve(OptimizeMojo.OPTIMIZED)
                .resolve(hash)
                .resolve("foo/x/main.xmir").toFile(),
            FileMatchers.anExistingFile()
        );
    }

    @Test
    void optimizesSuccessfully(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp);
        final Map<String, Path> res = maven.withProgram(
                "+package f",
                "[args] > main",
                "  (stdout \"Hello!\").print > @"
            )
            .with("trackOptimizationSteps", true)
            .execute(ParseMojo.class)
            .execute(OptimizeMojo.class)
            .result();
        MatcherAssert.assertThat(
            res,
            Matchers.hasKey(
                String.format("target/%s/foo/x/main/00-not-empty-atoms.xml", OptimizeMojo.STEPS)
            )
        );
        MatcherAssert.assertThat(
            res,
            Matchers.hasKey(
                String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
            )
        );
    }

    @Test
    void failsOnErrorFlag(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .withProgram(
                    "+package f",
                    "+alias THIS-IS-WRONG org.eolang.io.stdout",
                    "[args] > main",
                    "  (stdout \"Hello!\").print > @"
                )
                .with("failOnError", false)
                .execute(ParseMojo.class)
                .execute(OptimizeMojo.class)
                .result(),
            Matchers.not(
                Matchers.hasKey(
                    String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
                )
            )
        );
    }

    @Test
    void testOptimizedFail(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo/main.eo");
        new Home(temp).save(
            String.join(
                "\n",
                "+package f",
                "\n+alias THIS-IS-WRONG org.eolang.io.stdout",
                "[args] > main",
                "  (stdout \"Hello!\").print > @\n"
            ),
            src
        );
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.csv");
        Catalogs.INSTANCE.make(foreign)
            .add("foo.main")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("cache", temp.resolve("cache/parsed"))
            .execute();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Moja<>(OptimizeMojo.class)
                .with("targetDir", target.toFile())
                .with("foreign", foreign.toFile())
                .with("foreignFormat", "csv")
                .execute()
        );
    }

    @Test
    void stopsOnCritical(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo/main.eo");
        new Home(temp).save(
            String.join(
                "\n",
                "+package f\n",
                "[args] > main",
                "  seq > @",
                "    TRUE > x",
                "    FALSE > x\n"
            ),
            src
        );
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.csv");
        Catalogs.INSTANCE.make(foreign)
            .add("foo.main")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("cache", temp.resolve("cache/parsed"))
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("failOnError", false)
            .with("trackOptimizationSteps", true)
            .execute();
        MatcherAssert.assertThat(
            new XMLDocument(
                target.resolve(
                    String.format("%s/foo/main/02-duplicate-names.xml", OptimizeMojo.STEPS)
                )
            ),
            XhtmlMatchers.hasXPaths(
                "/program/sheets[count(sheet)=3]",
                "/program/errors[count(error)=1]",
                "/program/errors/error[@severity='critical']"
            )
        );
    }

    @Test
    void failsOnCritical(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo/main.eo");
        new Home(temp).save(
            String.join(
                "\n",
                "+package f\n",
                "[args] > main",
                "  seq > @",
                "    TRUE > x",
                "    FALSE > x\n"
            ),
            src
        );
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.csv");
        Catalogs.INSTANCE.make(foreign)
            .add("foo.main")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("cache", temp.resolve("cache/parsed"))
            .execute();
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Moja<>(OptimizeMojo.class)
                .with("targetDir", target.toFile())
                .with("foreign", foreign.toFile())
                .with("foreignFormat", "csv")
                .execute()
        );
    }

    @Test
    void testFailOnWarning(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo.src.eo");
        new Home(temp).save(
            new ResourceOf("org/eolang/maven/withwarning.eo"),
            temp.relativize(src)
        );
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        Catalogs.INSTANCE.make(foreign)
            .add("foo.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("cache", temp.resolve("cache/parsed"))
            .with("foreignFormat", "csv")
            .execute();
        this.applyXsl(
            "org/eolang/maven/set-warning-severity.xsl",
            target.resolve("01-parse/foo/src.xmir")
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new Moja<>(OptimizeMojo.class)
                .with("targetDir", target.toFile())
                .with("foreign", foreign.toFile())
                .with("foreignFormat", "csv")
                .with("failOnError", false)
                .with("failOnWarning", true)
                .execute()
        );
    }

    /**
     * Apply XSL transformation.
     * @param xsl Path to XSL within classpath
     * @param xml Path to XML to be tranformed
     */
    private void applyXsl(final String xsl, final Path xml) throws Exception {
        final XML output = new Xsline(
            new TrDefault<Shift>()
                .with(
                    new StXSL(
                        new XSLDocument(
                            new ResourceOf(xsl).stream()
                        )))
        ).pass(new XMLDocument(xml));
        new Home(xml.getParent()).save(output.toString(), xml.getParent().relativize(xml));
    }
}
