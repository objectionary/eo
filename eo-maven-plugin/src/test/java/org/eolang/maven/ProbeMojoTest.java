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

import com.yegor256.tojos.MnJson;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.LinkedList;
import org.cactoos.Input;
import org.cactoos.func.UncheckedFunc;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.map.MapEntry;
import org.cactoos.text.TextOf;
import org.eolang.maven.objectionary.Objectionary;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link ProbeMojo}.
 *
 * @since 0.28.11
 */
final class ProbeMojoTest {

    /**
     * Default format of eo-foreign.json for all tests.
     */
    private static final String FOREIGN_FORMAT = "json";

    @Test
    void testSimpleProbe(@TempDir final Path temp) throws IOException {
        final Input src = new InputOf(
            new TextOf(
                new ResourceOf("org/eolang/maven/simple-io.eo")
            )
        );
        this.saveProgram(temp, src);
        this.execUntilProbeMojo(temp);
        final File target = temp.resolve("target").toFile();
        final File foreign = temp.resolve(
            String.format("eo-foreign.%s", ProbeMojoTest.FOREIGN_FORMAT)
        ).toFile();
        new Moja<>(ProbeMojo.class)
            .with("targetDir", target)
            .with("foreign", foreign)
            .with("foreignFormat", ProbeMojoTest.FOREIGN_FORMAT)
            .with("objectionary", this.dummy())
            .execute();
        MatcherAssert.assertThat(
            new LinkedList<>(new MnJson(foreign).read()).getFirst().get("probed"),
            Matchers.equalTo("7")
        );
    }

    @Test
    void probeUsingOfflineHashFile(@TempDir final Path temp) throws IOException {
        new Home().save(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            temp.resolve("tags.txt")
        );
        final Input src = new InputOf(
            new TextOf(
                new ResourceOf("org/eolang/maven/simple-io.eo")
            )
        );
        this.saveProgram(temp, src);
        this.execUntilProbeMojo(temp);
        final File target = temp.resolve("target").toFile();
        final File foreign = temp.resolve(
            String.format("eo-foreign.%s", ProbeMojoTest.FOREIGN_FORMAT)
        ).toFile();
        new Moja<>(ProbeMojo.class)
            .with("targetDir", target)
            .with("foreign", foreign)
            .with("foreignFormat", ProbeMojoTest.FOREIGN_FORMAT)
            .with("objectionary", this.dummy())
            .with("offlineHashFile", temp.resolve("tags.txt"))
            .execute();
        MatcherAssert.assertThat(
            new LinkedList<>(new MnJson(foreign).read()).getFirst().get("hash"),
            Matchers.equalTo("mmmmmmm")
        );
    }

    @Test
    void probeUsingOfflineHash(@TempDir final Path temp) throws IOException {
        final Input src = new InputOf(
            new TextOf(
                new ResourceOf("org/eolang/maven/simple-io.eo")
            )
        );
        this.saveProgram(temp, src);
        this.execUntilProbeMojo(temp);
        final File target = temp.resolve("target").toFile();
        final File foreign = temp.resolve(
            String.format("eo-foreign.%s", ProbeMojoTest.FOREIGN_FORMAT)
        ).toFile();
        new Moja<>(ProbeMojo.class)
            .with("targetDir", target)
            .with("foreign", foreign)
            .with("foreignFormat", ProbeMojoTest.FOREIGN_FORMAT)
            .with("objectionary", this.dummy())
            .with("tag", "1.0.0")
            .with("offlineHash", "*.*.*:abcdefg")
            .execute();
        MatcherAssert.assertThat(
            new LinkedList<>(new MnJson(foreign).read()).getFirst().get("hash"),
            Matchers.equalTo("abcdefg")
        );
    }

    @Test
    void tryToFindInOyRemote(@TempDir final Path temp) throws IOException {
        final Input src = new InputOf(
            new TextOf(
                new ResourceOf("org/eolang/maven/simple-io.eo")
            )
        );
        this.saveProgram(temp, src);
        this.execUntilProbeMojo(temp);
        final File target = temp.resolve("target").toFile();
        final File foreign = temp.resolve(
            String.format("eo-foreign.%s", ProbeMojoTest.FOREIGN_FORMAT)
        ).toFile();
        final MapEntry<Objectionary, Boolean> entry = PullMojo.remote(
            PullMojo.resolveHash(null, null, "0.28.10")
        );
        new Moja<>(ProbeMojo.class)
            .with("targetDir", target)
            .with("foreign", foreign)
            .with("foreignFormat", ProbeMojoTest.FOREIGN_FORMAT)
            .with("tag", "0.28.10")
            .with("objectionary", entry.getKey())
            .execute();
        if (entry.getValue().equals(true)) {
            MatcherAssert.assertThat(
                new LinkedList<>(new MnJson(foreign).read()).getFirst().get("probed"),
                Matchers.equalTo("2")
            );
        }
    }

    private void execUntilProbeMojo(final Path temp) {
        final File target = temp.resolve("target").toFile();
        final File foreign = temp.resolve(
            String.format("eo-foreign.%s", ProbeMojoTest.FOREIGN_FORMAT)
        ).toFile();
        new Moja<>(ParseMojo.class)
            .with("targetDir", target)
            .with("foreign", foreign)
            .execute();
        new Moja<>(OptimizeMojo.class)
            .with("targetDir", target)
            .with("foreign", foreign)
            .execute();
        new Moja<>(DiscoverMojo.class)
            .with("targetDir", target)
            .with("foreign", foreign)
            .execute();
    }

    private void saveProgram(final Path temp, final Input code) throws IOException {
        final Path program = temp.resolve("program.eo");
        new Home(temp).save(code, temp.relativize(program));
        Catalogs.INSTANCE.make(
                temp.resolve(String.format("eo-foreign.%s", ProbeMojoTest.FOREIGN_FORMAT)),
                "json"
            )
            .add("foo.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, program.toString());
    }

    /**
     * Dummy Objectionary.
     *
     * @return Dummy Objectionary.
     */
    private Objectionary dummy() {
        return new OyLambda(
            new UncheckedFunc<>(s -> new InputOf("[] > hello\n"))
        );
    }

}
