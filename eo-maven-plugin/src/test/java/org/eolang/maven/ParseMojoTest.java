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

import com.yegor256.tojos.Csv;
import com.yegor256.tojos.MonoTojos;
import com.yegor256.tojos.SmartTojos;
import java.nio.file.Files;
import java.nio.file.Path;
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
public final class ParseMojoTest {

    @Test
    public void testSimpleParsing(@TempDir final Path temp) throws Exception {
        final Path src = temp.resolve("foo/x/main.eo");
        final Path target = temp.resolve("target");
        new Save(
            "+package f\n\n[args] > main\n  (stdout \"Hello!\").print\n",
            src
        ).save();
        final Path foreign = temp.resolve("eo-foreign.json");
        new MonoTojos(new Csv(foreign))
            .add("foo.x.main")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .execute();
        MatcherAssert.assertThat(
            Files.exists(
                target.resolve(
                    String.format("%s/foo/x/main.%s", ParseMojo.DIR, Transpiler.EXT)
                )
            ),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new SmartTojos(
                new MonoTojos(new Csv(foreign))
            ).getById("foo.x.main").exists("xmir"),
            Matchers.is(true)
        );
    }

    @Test
    public void testCrashOnInvalidSyntax(@TempDir final Path temp)
        throws Exception {
        final Path src = temp.resolve("bar/src.eo");
        new Save("something < is wrong here", src).save();
        final Path foreign = temp.resolve("foreign-1.json");
        new MonoTojos(new Csv(foreign))
            .add("bar.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Moja<>(ParseMojo.class)
                .with("targetDir", temp.resolve("target").toFile())
                .with("foreign", foreign.toFile())
                .with("foreignFormat", "csv")
                .execute()
        );
    }

    @Test
    public void testCrashesWithFileName(@TempDir final Path temp)
        throws Exception {
        final Path src = temp.resolve("bar/src.eo");
        new Save("something < is wrong here", src).save();
        final Path foreign = temp.resolve("foreign-1.json");
        new MonoTojos(new Csv(foreign))
            .add("bar.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        final IllegalArgumentException exception = Assertions.assertThrows(
            IllegalArgumentException.class,
            () -> new Moja<>(ParseMojo.class)
                .with("targetDir", temp.resolve("target").toFile())
                .with("foreign", foreign.toFile())
                .with("foreignFormat", "csv")
                .execute()
        );
        Assertions.assertEquals(exception.getMessage(), String.format("Failed to parse %s", src));
    }

    @Test
    public void testDoNotCrashesWithFailOnError(@TempDir final Path temp)
        throws Exception {
        final Path src = temp.resolve("foo/x/main.eo");
        final Path target = temp.resolve("target");
        new Save(
            "something < is wrong here",
            src
        ).save();
        final Path foreign = temp.resolve("eo-foreign.json");
        new MonoTojos(new Csv(foreign))
            .add("foo.x.main")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, src.toString());
        new Moja<>(ParseMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", "csv")
            .with("failOnError", false)
            .execute();
        MatcherAssert.assertThat(
            Files.notExists(
                target.resolve(
                    String.format("%s/foo/x/main.%s", ParseMojo.DIR, Transpiler.EXT)
                )
            ),
            Matchers.is(true)
        );
    }

}
