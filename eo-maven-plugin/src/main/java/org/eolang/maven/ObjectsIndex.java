/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.aspects.RetryOnFailure;
import java.net.URL;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import org.cactoos.Scalar;
import org.cactoos.Text;
import org.cactoos.iterable.Mapped;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.cactoos.set.SetOf;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;

/**
 * The objects index that contains all available EO objects.
 *
 * @see <a href="https://github.com/objectionary/home/blob/gh-pages/objectionary.lst">Example</a>
 * @since 0.29
 */
final class ObjectsIndex {

    /**
     * Tags.
     */
    private static final String HOME = "https://home.objectionary.com/objectionary.lst";

    /**
     * Cached objects index.
     */
    private final Scalar<? extends Set<String>> objects;

    /**
     * Ctor.
     */
    ObjectsIndex() {
        this(
            () -> new SetOf<>(
                new Mapped<>(
                    ObjectsIndex::convert,
                    new Mapped<>(
                        Text::asString,
                        new Split(
                            ObjectsIndex.asText(new URL(ObjectsIndex.HOME)),
                            "\n"
                        )
                    )
                )
            )
        );
    }

    /**
     * Ctor.
     * @param all All objects index.
     */
    ObjectsIndex(final Scalar<? extends Set<String>> all) {
        this.objects = new Sticky<>(all);
    }

    /**
     * Checks whether object index contains the object.
     *
     * @param name Object name.
     * @return True if object index contains the object.
     * @throws Exception If something unexpected happened.
     */
    public boolean contains(final String name) throws Exception {
        return this.objects.value().contains(name);
    }

    /**
     * Converts object name to the format that is used in the objectionary.
     * - "objects/org/eolang/array.eo" -> "org.eolang.array"
     * - "tests/org/eolang/seq-tests.eo" -> "org.eolang.seq-tests"
     * @param name Object name in raw format.
     * @return Object name in objectionary format.
     */
    private static String convert(final String name) {
        return name.substring(0, name.length() - 3)
            .replace('/', '.')
            .substring(name.indexOf('/') + 1);
    }

    /**
     * Download from the URL and return the content.
     * @param url The URL with tags
     * @return The body of the web page
     */
    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private static Text asText(final URL url) {
        return new TextOf(
            new Unchecked<>(() -> new TextOf(url).asString())
        );
    }
}
