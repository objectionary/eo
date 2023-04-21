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

import com.yegor256.tojos.MnJson;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.eolang.maven.hash.ChCompound;
import org.eolang.maven.objectionary.Objectionary;
import org.eolang.maven.objectionary.OyRemote;
import org.eolang.maven.tojos.ForeignTojos;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Online;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link PullMojo}.
 *
 * @since 0.1
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
@ExtendWith(OnlineCondition.class)
final class PullMojoTest {

    /**
     * Default format of eo-foreign.json for all tests.
     */
    private static final String FOREIGN_FORMAT = "json";

    @Test
    void pullsSuccessfully(@TempDir final Path temp) throws IOException {
        final FakeMaven maven = new FakeMaven(temp);
        maven.foreign()
            .add("org.eolang.io.stdout")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_VERSION, "*.*.*");
        maven.with("objectionary", this.dummy())
            .execute(PullMojo.class);
        MatcherAssert.assertThat(
            new Home(temp.resolve("target")).exists(
                Paths.get(
                    String.format(
                        "%s/org/eolang/io/stdout.eo",
                        PullMojo.DIR
                    )
                )
            ),
            Matchers.is(true)
        );
    }

    @Test
    void pullsFromProbes(@TempDir final Path temp) throws IOException {
        final Path program = temp.resolve("program.eo");
        new Home(temp).save(
            new InputOf(
                new TextOf(
                    new ResourceOf("org/eolang/maven/simple-io.eo")
                )
            ),
            temp.relativize(program)
        );
        Catalogs.INSTANCE.make(temp.resolve("eo-foreign.json"), "json")
            .add("foo.src")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_EO, program.toString());
        final File target = temp.resolve("target").toFile();
        final File foreign = temp.resolve("eo-foreign.json").toFile();
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
        final Objectionary objectionary = new OyRemote(
            new ChCompound(null, null, "master")
        );
        new Moja<>(ProbeMojo.class)
            .with("targetDir", target)
            .with("foreign", foreign)
            .with("foreignFormat", "json")
            .with("objectionary", objectionary)
            .execute();
        new Moja<>(PullMojo.class)
            .with("targetDir", target)
            .with("foreign", foreign)
            .with("foreignFormat", PullMojoTest.FOREIGN_FORMAT)
            .with("objectionary", objectionary)
            .execute();
        MatcherAssert.assertThat(
            new Home(target.toPath()).exists(
                Paths.get(
                    String.format(
                        "%s/org/eolang/io/stdout.eo",
                        PullMojo.DIR
                    )
                )
            ),
            Matchers.is(new Online().value())
        );
    }

    @Test
    void pullsUsingOfflineHashFile(@TempDir final Path temp) throws IOException {
        new Home(temp).save(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            Paths.get("tags.txt")
        );
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        Catalogs.INSTANCE.make(foreign, PullMojoTest.FOREIGN_FORMAT)
            .add("org.eolang.io.stdout")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_VERSION, "*.*.*");
        new Moja<>(PullMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", PullMojoTest.FOREIGN_FORMAT)
            .with("objectionary", this.dummy())
            .with("offlineHashFile", temp.resolve("tags.txt"))
            .execute();
        MatcherAssert.assertThat(
            new LinkedList<>(new MnJson(foreign).read()).getFirst().get("hash"),
            Matchers.equalTo("mmmmmmm")
        );
    }

    /**
     * Offline hash test.
     *
     * @param temp Temporary directory for test.
     */
    @Test
    void pullsUsingOfflineHash(@TempDir final Path temp) {
        final Path target = temp.resolve("target");
        final Path foreign = temp.resolve("eo-foreign.json");
        Catalogs.INSTANCE.make(foreign, PullMojoTest.FOREIGN_FORMAT)
            .add("org.eolang.io.stdout")
            .set(AssembleMojo.ATTR_SCOPE, "compile")
            .set(AssembleMojo.ATTR_VERSION, "*.*.*");
        new Moja<>(PullMojo.class)
            .with("targetDir", target.toFile())
            .with("foreign", foreign.toFile())
            .with("foreignFormat", PullMojoTest.FOREIGN_FORMAT)
            .with("objectionary", this.dummy())
            .with("tag", "1.0.0")
            .with("offlineHash", "*.*.*:abcdefg")
            .execute();
        MatcherAssert.assertThat(
            new LinkedList<>(new MnJson(foreign).read()).getFirst().get("hash"),
            Matchers.equalTo("abcdefg")
        );
    }

    /**
     * Dummy Objectionary.
     *
     * @return Dummy Objectionary.
     */
    private Objectionary dummy() {
        return new OyFake();
    }

}
