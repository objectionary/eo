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

import com.yegor256.tojos.TjSmart;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.eolang.maven.hash.ChNarrow;
import org.eolang.maven.hash.ChRemote;
import org.eolang.maven.testapi.MwFake;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link ParseMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class ParseMojoTest {

    @Test
    void testSimpleParsing(@TempDir final Path temp) throws Exception {
        MatcherAssert.assertThat(
            new MwFake(temp)
                .program("+package f\n\n[args] > main\n  (stdout \"Hello!\").print\n")
                .execute(ParseMojo.class)
                .result()
                .xmirCompiled(),
            Matchers.is(true)
        );
    }

    @Test
    void testSimpleParsingCached(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo/x/main.eo");
        final Path target = temp.resolve("target");
        new Home(temp).save(
            "invalid content",
            temp.relativize(src)
        );
        final Path foreign = temp.resolve("eo-foreign.csv");
        new FtCached(
            new ChNarrow(new ChRemote("0.25.0")).value(),
            target,
            temp.resolve("parsed")
        ).save(
            "foo.x.main",
            "xmir",
            () -> new UncheckedText(
                new TextOf(new ResourceOf("org/eolang/maven/main.xmir"))
            ).asString()
        );
        Catalogs.INSTANCE.make(foreign)
            .add("foo.x.main")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString())
            .set(AssembleMojo.ATTR_VERSION, "0.25.0");
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("cache", temp.resolve("parsed"))
            .execute();
        MatcherAssert.assertThat(
            new Home(target).exists(
                Paths.get(
                    String.format("%s/foo/x/main.%s", ParseMojo.DIR, TranspileMojo.EXT)
                )
            ),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new TjSmart(
                Catalogs.INSTANCE.make(foreign)
            ).getById("foo.x.main").exists("xmir"),
            Matchers.is(true)
        );
    }

    @Test
    void testCrashOnInvalidSyntax(@TempDir final Path temp)
        throws Exception {
        final Path src = temp.resolve("bar/src.eo");
        new Home(temp).save("something < is wrong here", temp.relativize(src));
        final Path foreign = temp.resolve("foreign-1");
        Catalogs.INSTANCE.make(foreign)
            .add("bar.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Moja<>(ParseMojo.class)
                .with("targetDir", temp.resolve("target").toFile())
                .with("foreign", foreign.toFile())
                .with("cache", temp.resolve("cache/parsed"))
                .with("foreignFormat", "csv")
                .execute()
        );
    }

    @Test
    void testCrashesWithFileName(@TempDir final Path temp)
        throws Exception {
        final Path src = temp.resolve("bar/src.eo");
        new Home(temp).save("something < is wrong here", temp.relativize(src));
        final Path foreign = temp.resolve("foreign-1");
        Catalogs.INSTANCE.make(foreign)
            .add("bar.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        final IllegalArgumentException exception = Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Moja<>(ParseMojo.class)
                .with("targetDir", temp.resolve("target").toFile())
                .with("foreign", foreign.toFile())
                .with("cache", temp.resolve("cache/parsed"))
                .with("foreignFormat", "csv")
                .execute()
        );
        Assertions.assertEquals(String.format("Failed to parse %s", src), exception.getMessage());
    }

    @Test
    void testDoNotCrashesWithFailOnError(@TempDir final Path temp)
        throws Exception {
        final Path src = temp.resolve("foo/x/main.eo");
        final Path target = temp.resolve("target");
        new Home(temp).save(
            "something < is wrong here",
            temp.relativize(src)
        );
        final Path foreign = temp.resolve("eo-foreign");
        Catalogs.INSTANCE.make(foreign)
            .add("foo.x.main")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("cache", temp.resolve("cache/parsed"))
            .with("failOnError", false)
            .execute();
        MatcherAssert.assertThat(
            Files.notExists(
                target.resolve(
                    String.format("%s/foo/x/main.%s", ParseMojo.DIR, TranspileMojo.EXT)
                )
            ),
            Matchers.is(true)
        );
    }

}
