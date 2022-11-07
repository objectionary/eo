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
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import org.cactoos.io.InputOf;
import org.cactoos.io.ResourceOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link PullMojo}.
 *
 * @since 0.1
 */
final class PullMojoTest {

    /**
     * Default format of eo-foreign.json for all tests.
     */
    private static final String FOREIGN_FORMAT = "json";

    @Test
    void testSimplePull(@TempDir final Path temp) {
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
            .execute();
        MatcherAssert.assertThat(
            new Home(target).exists(
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
    void pullsUsingOfflineHashFile(@TempDir final Path temp) throws IOException {
        new Home().save(
            new ResourceOf("org/eolang/maven/commits/tags.txt"),
            temp.resolve("tags.txt")
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
        return input -> new InputOf("[] > hello\n");
    }
}
