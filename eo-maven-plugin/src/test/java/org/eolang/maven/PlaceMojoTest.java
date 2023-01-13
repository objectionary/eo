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

import com.yegor256.tojos.Tojos;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.eolang.maven.util.Home;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link PlaceMojo}.
 *
 * @since 0.11
 */
final class PlaceMojoTest {

    @Test
    void placesBinaries(@TempDir final Path temp) throws Exception {
        final Path bins = temp.resolve(ResolveMojo.DIR);
        final Path classes = temp.resolve("classes");
        new Home(bins).save("x1", Paths.get("foo/hello/-/0.1/EObar/x.bin"));
        new Home(bins).save("x2", Paths.get("foo/hello/-/0.1/org/eolang/f/x.a.class"));
        new Home(bins).save("x3", Paths.get("foo/hello/-/0.1/org/eolang/t.txt"));
        new Moja<>(PlaceMojo.class)
            .with("targetDir", temp.toFile())
            .with("outputDir", classes.toFile())
            .with("placed", temp.resolve("placed.json").toFile())
            .execute();
        MatcherAssert.assertThat(
            new Home(classes).exists(Paths.get("EObar/x.bin")),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new Home(classes).exists(classes.resolve("org/eolang/f/x.a.class")),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new Home(classes).exists(classes.resolve("org/eolang/t.txt")),
            Matchers.is(true)
        );
    }

    @Test
    void placesMissing(@TempDir final Path temp) throws Exception {
        final Path bins = temp.resolve(ResolveMojo.DIR);
        final Path classes = temp.resolve("classes");
        final Path placed = temp.resolve("placed.json");
        new Home(bins).save("x1", Paths.get("foo/hello/-/0.1/EObar/x.bin"));
        new Home(classes).save("x1", Paths.get("EObar/x.bin"));
        new Home(bins).save("x2", Paths.get("foo/hello/-/0.1/org/eolang/f/x.a.class"));
        new Moja<>(PlaceMojo.class)
            .with("targetDir", temp.toFile())
            .with("outputDir", classes.toFile())
            .with("placed", placed.toFile())
            .execute();
        final Tojos tojos = Catalogs.INSTANCE.make(placed, "csv");
        tojos.add("foo/hello/-/0.1/EObar/x.bin");
        tojos.add("foo/hello/-/0.1/org/eolang/f/x.a.class");
        MatcherAssert.assertThat(
            Files.exists(classes.resolve("EObar/x.bin")),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            Files.exists(classes.resolve("org/eolang/f/x.a.class")),
            Matchers.is(true)
        );
    }

    /**
     * Test case for {@link PlaceMojo#execute()}.
     * Since for tests we are using {@link org.eolang.maven.DummyCentral}, then instead of unpacking
     * of classes from jar it just copies the jar itself to target/classes folder.
     *
     * @param temp Temporary directory
     * @throws IOException If fails
     */
    @Test
    @ExtendWith(OnlineCondition.class)
    void placesAllEoRuntimeClasses(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            maven
                .withHelloWorld()
                .execute(new FakeMaven.Place())
                .result()
                .get("target/classes"),
            new ContainsFile("**/eo-runtime-*.jar")
        );
        MatcherAssert.assertThat(
            maven.placed().select(tojo -> "jar".equals(tojo.get(PlaceMojo.ATTR_PLD_KIND))).size(),
            Matchers.equalTo(1)
        );
    }

    @Test
    void placesWithoutEoRuntimeClasses(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        MatcherAssert.assertThat(
            maven
                .withHelloWorld()
                .with("withRuntimeDependency", false)
                .execute(new FakeMaven.Place())
                .result()
                .get("target/classes"),
            Matchers.not(new ContainsFile("**/eo-runtime-*.jar"))
        );
        MatcherAssert.assertThat(
            maven.placed().select(tojo -> "jar".equals(tojo.get(PlaceMojo.ATTR_PLD_KIND))).size(),
            Matchers.equalTo(0)
        );
    }

}
