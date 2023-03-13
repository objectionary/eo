/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.jucs.ClasspathSource;
import org.eolang.maven.util.Home;
import org.eolang.parser.CheckPack;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.hamcrest.io.FileMatchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;

/**
 * Test case for {@link OptimizeMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings({"PMD.AvoidDuplicateLiterals", "PMD.TooManyMethods"})
final class OptimizeMojoTest {

    @ParameterizedTest
    @ClasspathSource(value = "org/eolang/maven/packs/", glob = "**.yaml")
    void checksPacks(final String pack) throws Exception {
        MatcherAssert.assertThat(
            new CheckPack(pack).failures(),
            Matchers.empty()
        );
    }

    @Test
    void skipsAlreadyOptimized(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp)
            .withHelloWorld()
            .execute(new FakeMaven.Optimize());
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
            .withHelloWorld()
            .execute(new FakeMaven.Optimize())
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
            .withHelloWorld()
            .with("cache", cache)
            .withTojoAttribute(AssembleMojo.ATTR_HASH, hash)
            .execute(new FakeMaven.Optimize());
        MatcherAssert.assertThat(
            new XMLDocument(
                new Home(temp).load(
                    Paths.get(
                        String.format(
                            "target/%s/foo/x/main.%s",
                            OptimizeMojo.DIR,
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
            .withHelloWorld()
            .with("cache", cache)
            .withTojoAttribute(AssembleMojo.ATTR_HASH, hash)
            .execute(new FakeMaven.Optimize());
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
        final Map<String, Path> res = maven
            .withHelloWorld()
            .with("trackOptimizationSteps", true)
            .execute(new FakeMaven.Optimize())
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

    /**
     * The test with high number of eo programs reveals concurrency problems of the OptimizeMojo.
     * Since other tests works only with single program - it's hard to find concurrency mistakes.
     * @param temp Test directory.
     * @throws java.io.IOException If problem with filesystem happened.
     */
    @Test
    void optimizesConcurrentlyWithLotsOfPrograms(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        final int total = 20;
        for (int program = 0; program < total; ++program) {
            maven.withHelloWorld();
        }
        final Map<String, Path> res = maven
            .execute(new FakeMaven.Optimize())
            .result();
        for (int program = 0; program < total; ++program) {
            MatcherAssert.assertThat(
                res,
                Matchers.hasKey(
                    String.format(
                        "target/%s/foo/x/main%s.%s",
                        OptimizeMojo.DIR,
                        FakeMaven.suffix(program),
                        TranspileMojo.EXT
                    )
                )
            );
        }
    }

    @Test
    void failsOnErrorFlag(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "+alias THIS-IS-WRONG org.eolang.io.stdout",
                    "[args] > main",
                    "  (stdout \"Hello!\").print > @"
                )
                .with("failOnError", false)
                .execute(new FakeMaven.Optimize())
                .result(),
            Matchers.not(
                Matchers.hasKey(
                    String.format("target/%s/foo/x/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
                )
            )
        );
    }

    @Test
    void failsOptimization(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f",
                    "+alias THIS-IS-WRONG org.eolang.io.stdout\n",
                    "[args] > main",
                    "  (stdout \"Hello!\").print > @"
                )
                .execute(new FakeMaven.Optimize())
        );
    }

    @Test
    void stopsOnCritical(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            new XMLDocument(
                new FakeMaven(temp)
                    .withProgram(
                        "+package f\n",
                        "[args] > main",
                        "  seq > @",
                        "    TRUE > x",
                        "    FALSE > x"
                    )
                    .with("trackOptimizationSteps", true)
                    .with("failOnError", false)
                    .execute(new FakeMaven.Optimize())
                    .result()
                    .get(
                        String.format(
                            "target/%s/foo/x/main/02-duplicate-names.xml",
                            OptimizeMojo.STEPS
                        )
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
    void failsOnCritical(@TempDir final Path temp) {
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> new FakeMaven(temp)
                .withProgram(
                    "+package f\n",
                    "[args] > main",
                    "  seq > @",
                    "    TRUE > x",
                    "    FALSE > x"
                )
                .execute(new FakeMaven.Optimize())
        );
    }

    @Test
    void failsOnWarning(@TempDir final Path temp) throws Exception {
        final FakeMaven maven = new FakeMaven(temp)
            .withProgram(
                "+architect yegor256@gmail.com",
                "+junit",
                "+package org.eolang.examples\n",
                "[] > main",
                "  [] > @",
                "    hello > test"
            );
        this.applyXsl(
            "org/eolang/maven/set-warning-severity.xsl",
            maven.execute(ParseMojo.class)
                .result()
                .get("target/1-parse/foo/x/main.xmir")
        );
        Assertions.assertThrows(
            IllegalStateException.class,
            () -> maven.with("failOnError", false)
                .with("failOnWarning", true)
                .execute(OptimizeMojo.class)
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
