/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang.maven.objectionary;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import org.cactoos.io.InputOf;
import org.cactoos.text.TextOf;
import org.hamcrest.MatcherAssert;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/**
 * Test case for {@link OyFilesystem}.
 *
 * @since 0.30
 */
class OyFilesystemTest {

    /**
     * Object content.
     */
    private static final String OBJECT_CONTENT = "Object Content";

    @Test
    void containsObject(@TempDir final Path home) throws IOException {
        final OyFilesystem objectionary = new OyFilesystem(home);
        final String object = "org.eolang.found";
        OyFilesystemTest.save(object, home);
        MatcherAssert.assertThat(
            objectionary.contains(object),
            Matchers.is(true)
        );
    }

    @Test
    void containsObjectWithDefaultHome(@TempDir final Path home) throws IOException {
        final OyFilesystem objectionary = new OyFilesystem();
        final String object = "org.eolang.ram";
        OyFilesystemTest.save(object, home);
        MatcherAssert.assertThat(
            objectionary.contains(object),
            Matchers.is(true)
        );
    }

    @Test
    void doesNotContainObject() {
        final OyFilesystem objectionary = new OyFilesystem();
        final String looking = "org.eolang.not-found";
        MatcherAssert.assertThat(
            String.format(
                "We shouldn't find the %s object, but it was successfully found",
                looking
            ),
            objectionary.contains(looking),
            Matchers.is(false)
        );
    }

    @Test
    void getsObjectSuccessfully(@TempDir final Path home) throws IOException {
        final OyFilesystem objectionary = new OyFilesystem(home);
        final String object = "org.eolang.get";
        OyFilesystemTest.save(object, home);
        MatcherAssert.assertThat(
            new TextOf(objectionary.get(object)),
            Matchers.equalTo(new TextOf(new InputOf(OyFilesystemTest.OBJECT_CONTENT)))
        );
    }

    @Test
    void getsAbsentObject() {
        final OyFilesystem objectionary = new OyFilesystem();
        final String absent = "org.eolang.absent";
        Assertions.assertThrows(
            IOException.class,
            () -> new TextOf(objectionary.get(absent)).asString(),
            String.format(
                "The getting of %s object should thrown an exception, but noting happened",
                absent
            )
        );
    }

    /**
     * Saves object to the home directory.
     * @param object Object name
     * @param home Home directory
     * @throws IOException If some I/O problem occurs
     */
    private static void save(final String object, final Path home) throws IOException {
        final Path name = Arrays.stream(object.split("\\."))
            .map(Paths::get)
            .reduce(home.resolve(OyFilesystem.SOURCES), Path::resolve);
        Files.createDirectories(name.getParent());
        Files.write(
            name.resolveSibling(String.format("%s.%s", name.getFileName(), "eo")),
            OyFilesystemTest.OBJECT_CONTENT.getBytes(StandardCharsets.UTF_8),
            StandardOpenOption.CREATE
        );
    }
}
