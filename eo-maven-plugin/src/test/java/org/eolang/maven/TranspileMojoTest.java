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

import com.jcabi.log.Logger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.apache.maven.plugin.testing.stubs.MavenProjectStub;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link TranspileMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class TranspileMojoTest {
    @Test
    void recompileIfModified(@TempDir final Path temp)
        throws Exception {
        final Input source = new ResourceOf("org/eolang/maven/mess.eo");
        final Path src = temp.resolve("foo.src.eo");
        new Home(temp).save(source, temp.relativize(src));
        final Path target = temp.resolve("target");
        final Path generated = temp.resolve("generated");
        final Path foreign = temp.resolve("eo-foreign.json");
        final Path transpiled = temp.resolve("eo-transpiled.json");
        Catalogs.INSTANCE.make(foreign)
            .add("foo.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(TranspileMojo.class)
            .with("project", new MavenProjectStub())
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("transpiled", transpiled.toFile())
            .with("transpiledFormat", "csv")
            .execute();
        final Path java = generated.resolve("EOorg/EOeolang/EOexamples/EOmessTest.java");
        MatcherAssert.assertThat(
            String.format("The file \"%s\" wasn't created", java),
            new Home(temp).exists(temp.relativize(java)),
            Matchers.is(true)
        );
        final long before = java.toFile().lastModified();
        Assertions.assertTrue(src.toFile().setLastModified(before + 1));
        new Moja<>(TranspileMojo.class)
            .with("project", new MavenProjectStub())
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("transpiled", transpiled.toFile())
            .with("transpiledFormat", "csv")
            .execute();
        MatcherAssert.assertThat(
            java.toFile().lastModified(),
            Matchers.greaterThan(before)
        );
    }

    @Test
    void recompilesIfExpired(@TempDir final Path temp)
        throws Exception {
        final Input source = new ResourceOf("org/eolang/maven/mess.eo");
        final Path src = temp.resolve("foo.src.eo");
        new Home(temp).save(source, temp.relativize(src));
        final Path target = temp.resolve("target");
        final Path generated = temp.resolve("generated");
        final Path foreign = temp.resolve("eo-foreign.json");
        final Path transpiled = temp.resolve("eo-transpiled");
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
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(TranspileMojo.class)
            .with("project", new MavenProjectStub())
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("transpiled", transpiled.toFile())
            .with("transpiledFormat", "csv")
            .execute();
        final Path java = generated.resolve("EOorg/EOeolang/EOexamples/EOmessTest.java");
        MatcherAssert.assertThat(
            String.format("The file \"%s\" wasn't created", java),
            new Home(temp).exists(temp.relativize(java)),
            Matchers.is(true)
        );
        Assertions.assertTrue(java.toFile().setLastModified(0L));
        final Path xmir = target.resolve("06-transpile")
            .resolve("foo")
            .resolve("src.xmir");
        Assertions.assertTrue(new Home(temp).exists(temp.relativize(xmir)));
        Assertions.assertTrue(xmir.toFile().setLastModified(0L));
        new Moja<>(TranspileMojo.class)
            .with("project", new MavenProjectStub())
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("transpiled", transpiled.toFile())
            .with("transpiledFormat", "csv")
            .execute();
        MatcherAssert.assertThat(
            java.toFile().lastModified(),
            Matchers.greaterThan(0L)
        );
    }

    @Test
    void notRecompileIfNotModified(@TempDir final Path temp)
        throws Exception {
        final Input source = new ResourceOf("org/eolang/maven/mess.eo");
        final Path src = temp.resolve("foo.src.eo");
        new Home(temp).save(source, temp.relativize(src));
        final Path target = temp.resolve("target");
        final Path generated = temp.resolve("generated");
        final Path foreign = temp.resolve("eo-foreign.json");
        final Path transpiled = temp.resolve("eo-transpiled");
        Catalogs.INSTANCE.make(foreign)
            .add("foo.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(TranspileMojo.class)
            .with("project", new MavenProjectStub())
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("transpiled", transpiled.toFile())
            .with("transpiledFormat", "csv")
            .execute();
        final Path java = generated.resolve("EOorg/EOeolang/EOexamples/EOmessTest.java");
        MatcherAssert.assertThat(
            String.format("The file \"%s\" wasn't created", java),
            new Home(temp).exists(temp.relativize(java)),
            Matchers.is(true)
        );
        Assertions.assertTrue(java.toFile().setLastModified(0L));
        final Path xmir = target.resolve("06-transpile")
            .resolve("foo")
            .resolve("src.xmir");
        Assertions.assertTrue(new Home(temp).exists(temp.relativize(xmir)));
        new Moja<>(TranspileMojo.class)
            .with("project", new MavenProjectStub())
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        MatcherAssert.assertThat(
            java.toFile().lastModified(),
            Matchers.equalTo(0L)
        );
    }

    @Test
    void testSimpleCompilation(@TempDir final Path temp)
        throws Exception {
        final String java = this.compile(
            temp,
            new ResourceOf("org/eolang/maven/mess.eo"),
            "EOorg/EOeolang/EOexamples/EOmessTest.java"
        );
        MatcherAssert.assertThat(
            java, Matchers.containsString("class EOmessTest")
        );
    }

    @Test
    void testRealCompilation(@TempDir final Path temp)
        throws Exception {
        final Path src = Paths.get("../eo-runtime/src/main/eo/org/eolang/array.eo");
        Assumptions.assumeTrue(Files.exists(src));
        final String java = this.compile(
            temp,
            new InputOf(src),
            "EOorg/EOeolang/EOarray.java"
        );
        MatcherAssert.assertThat(java, Matchers.containsString("class"));
    }

    /**
     * Compile EO to Java file.
     * @param temp Temp dir
     * @param code EO sources
     * @param file The file to return
     * @return All Java code
     * @throws Exception If fails
     */
    private Path compileToFile(
        final Path temp,
        final Input code,
        final String file
    ) throws Exception {
        final Path src = temp.resolve("foo.src.eo");
        new Home(temp).save(code, temp.relativize(src));
        final Path target = temp.resolve("target");
        final Path generated = temp.resolve("generated");
        final Path foreign = temp.resolve("eo-foreign.json");
        final Path transpiled = temp.resolve("eo-transpiled");
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
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        new Moja<>(TranspileMojo.class)
            .with("project", new MavenProjectStub())
            .with("targetDir", target.toFile())
            .with("generatedDir", generated.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("transpiled", transpiled.toFile())
            .with("transpiledFormat", "csv")
            .execute();
        final Path java = generated.resolve(file);
        MatcherAssert.assertThat(
            String.format("The file \"%s\" wasn't created", java),
            Files.exists(java),
            Matchers.is(true)
        );
        return java;
    }

    /**
     * Compile EO to Java.
     * @param temp Temp dir
     * @param code EO sources
     * @param file The file to return
     * @return All Java code
     * @throws Exception If fails
     */
    private String compile(
        final Path temp,
        final Input code,
        final String file
    ) throws Exception {
        final Path java = this.compileToFile(temp, code, file);
        final String out = new TextOf(new InputOf(java)).asString();
        Logger.debug(this, "Java output:\n%s", out);
        return out;
    }
}
