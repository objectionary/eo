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

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.jcabi.xml.XSLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.StXSL;
import com.yegor256.xsline.TrDefault;
import com.yegor256.xsline.Xsline;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.concurrent.TimeUnit;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
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
        final Path src = temp.resolve("foo/main.eo");
        new Home().save(
            "+package f\n\n[args] > main\n  (stdout \"Hello!\").print > @\n",
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
            .with("cache", temp.resolve("cache/parsed"))
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        final Path tgt = target.resolve(
            String.format("%s/foo/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
        );
        final long mtime = tgt.toFile().lastModified();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        MatcherAssert.assertThat(
            tgt.toFile().lastModified(),
            Matchers.is(mtime)
        );
    }

    @Test
    void optimizesIfExpired(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo/main.eo");
        new Home().save(
            "+package f\n\n[args] > main\n  (stdout \"Hello!\").print > @\n",
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
            .execute();
        final Path tgt = target.resolve(
            String.format("%s/foo/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
        );
        final long start = System.currentTimeMillis();
        tgt.toFile().setLastModified(start - TimeUnit.SECONDS.toMillis(10L));
        src.toFile().setLastModified(start - TimeUnit.SECONDS.toMillis(1L));
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        MatcherAssert.assertThat(
            tgt.toFile().lastModified(),
            Matchers.greaterThan(start)
        );
    }

    @Test
    void testSimpleOptimize(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo/main.eo");
        new Home().save(
            "+package f\n\n[args] > main\n  (stdout \"Hello!\").print > @\n",
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
            .with("trackOptimizationSteps", true)
            .with("foreignFormat", "csv")
            .execute();
        MatcherAssert.assertThat(
            new Home().exists(
                target.resolve(
                    String.format("%s/foo/main/00-not-empty-atoms.xml", OptimizeMojo.STEPS)
                )
            ),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new Home().exists(
                target.resolve(
                    String.format("%s/foo/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void testOptimizeWithFailOnErrorFlag(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo/main.eo");
        new Home().save(
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
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("failOnError", false)
            .execute();
        MatcherAssert.assertThat(
            Files.notExists(
                target.resolve(
                    String.format("%s/foo/main.%s", OptimizeMojo.DIR, TranspileMojo.EXT)
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void testOptimizedFail(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo/main.eo");
        new Home().save(
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
            IllegalArgumentException.class,
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
        new Home().save(new ResourceOf("org/eolang/maven/withwarning.eo"), src);
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
            IllegalArgumentException.class,
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
        new Home().save(output.toString(), xml);
    }
}
