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

import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.set.SetOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link UnplaceMojo}.
 *
 * @since 0.1
 * @checkstyle LocalFinalVariableNameCheck (100 lines)
 */
final class UnplaceMojoTest {
    /**
     * Value for 'class' ATTR_KIND.
     */
    private static final String ATTR_KIND_CLASS = "class";

    @Test
    void testCleaning(@TempDir final Path temp) throws Exception {
        final Path foo = temp.resolve("a/b/c/foo.class");
        new Home().save("...", foo);
        final Path pparent = foo.getParent().getParent();
        final Path foo2 = temp.resolve("a/b/c/foo2.class");
        new Home().save("...", foo2);
        final Path foo3 = temp.resolve("a/b/c/d/foo3.class");
        new Home().save("...", foo3);
        final Path foo4 = temp.resolve("a/b/c/e/foo4.class");
        new Home().save("...", foo4);
        final Path list = temp.resolve("placed.csv");
        Catalogs.INSTANCE.make(list)
            .add(foo.toString())
            .set(PlaceMojo.ATTR_KIND, UnplaceMojoTest.ATTR_KIND_CLASS)
            .set(PlaceMojo.ATTR_RELATED, "---")
            .set(PlaceMojo.ATTR_ORIGIN, "some.jar")
            .set(PlaceMojo.ATTR_HASH, new FileHash(foo));
        Catalogs.INSTANCE.make(list)
            .add(foo2.toString())
            .set(PlaceMojo.ATTR_KIND, UnplaceMojoTest.ATTR_KIND_CLASS)
            .set(PlaceMojo.ATTR_RELATED, "---")
            .set(PlaceMojo.ATTR_ORIGIN, "some.jar")
            .set(PlaceMojo.ATTR_HASH, new FileHash(foo2));
        Catalogs.INSTANCE.make(list)
            .add(foo3.toString())
            .set(PlaceMojo.ATTR_KIND, UnplaceMojoTest.ATTR_KIND_CLASS)
            .set(PlaceMojo.ATTR_RELATED, "---")
            .set(PlaceMojo.ATTR_ORIGIN, "some.jar")
            .set(PlaceMojo.ATTR_HASH, new FileHash(foo3));
        Catalogs.INSTANCE.make(list)
            .add(foo4.toString())
            .set(PlaceMojo.ATTR_KIND, UnplaceMojoTest.ATTR_KIND_CLASS)
            .set(PlaceMojo.ATTR_RELATED, "---")
            .set(PlaceMojo.ATTR_ORIGIN, "some.jar")
            .set(PlaceMojo.ATTR_HASH, new FileHash(foo4));
        new Moja<>(UnplaceMojo.class)
            .with("placed", list.toFile())
            .with("placedFormat", "csv")
            .execute();
        MatcherAssert.assertThat(
            new Home().exists(foo),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            new Home().exists(foo2),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            new Home().exists(foo3),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            new Home().exists(foo4),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            new Home().exists(Paths.get(String.valueOf(pparent))),
            Matchers.is(false)
        );
    }

    @Test
    void testKeepBinaries(@TempDir final Path temp) throws Exception {
        final Path foo = temp.resolve("a/b/c/foo5.class");
        new Home().save("testKeepBinaries", foo);
        final Path pparent = foo.getParent().getParent();
        final Path list = temp.resolve("placed.csv");
        Catalogs.INSTANCE.make(list)
            .add(foo.toString())
            .set(PlaceMojo.ATTR_KIND, UnplaceMojoTest.ATTR_KIND_CLASS)
            .set(PlaceMojo.ATTR_RELATED, "a/b/c/foo5.class")
            .set(PlaceMojo.ATTR_ORIGIN, "some-keep.jar")
            .set(PlaceMojo.ATTR_HASH, new FileHash(foo));
        new Moja<>(UnplaceMojo.class)
            .with("placed", list.toFile())
            .with("placedFormat", "csv")
            .with("keepBinaries", new SetOf<>("**foo5.class"))
            .execute();
        MatcherAssert.assertThat(
            new Home().exists(foo),
            Matchers.is(true)
        );
        MatcherAssert.assertThat(
            new Home().exists(Paths.get(String.valueOf(pparent))),
            Matchers.is(true)
        );
    }

    @Test
    void testKeepRemoveBinaries(@TempDir final Path temp) throws Exception {
        final Path foo = temp.resolve("a/b/c/foo6.class");
        new Home().save("testKeepRemoveBinaries", foo);
        final Path pparent = foo.getParent().getParent();
        final Path list = temp.resolve("placed.csv");
        Catalogs.INSTANCE.make(list)
            .add(foo.toString())
            .set(PlaceMojo.ATTR_KIND, UnplaceMojoTest.ATTR_KIND_CLASS)
            .set(PlaceMojo.ATTR_RELATED, "a/b/c/foo6.class")
            .set(PlaceMojo.ATTR_ORIGIN, "some-keep-remove.jar")
            .set(PlaceMojo.ATTR_HASH, new FileHash(foo));
        new Moja<>(UnplaceMojo.class)
            .with("placed", list.toFile())
            .with("placedFormat", "csv")
            .with("keepBinaries", new SetOf<>("**foo6.class"))
            .with("removeBinaries", new SetOf<>("**foo6.class"))
            .execute();
        MatcherAssert.assertThat(
            new Home().exists(foo),
            Matchers.is(false)
        );
        MatcherAssert.assertThat(
            new Home().exists(Paths.get(String.valueOf(pparent))),
            Matchers.is(false)
        );
    }
}
