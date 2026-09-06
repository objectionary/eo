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
import org.cactoos.iterable.Filtered;
import org.cactoos.iterable.Mapped;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.set.SetOf;
import org.cactoos.text.Split;
import org.cactoos.text.TextOf;

/**
 * The objects index that contains all available EO objects.
 * @see <a href="https://github.com/objectionary/home/blob/gh-pages/objectionary.lst">Example</a>
 * @since 0.29
 */
final class ObjectsIndex {

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
                            ObjectsIndex.asText(
                                new URL("https://home.objectionary.com/objectionary.lst")
                            ),
                            "\\n"
                        )
                    )
                )
            )
        );
    }

    /**
     * Ctor.
     * @param all All objects index
     */
    ObjectsIndex(final Scalar<? extends Set<String>> all) {
        this.objects = new Synced<>(new Sticky<>(all));
    }

    /**
     * Checks whether object index contains the object.
     * @param name Object name
     * @return True if object index contains the object
     * @throws Exception If something unexpected happened.
     */
    boolean contains(final String name) throws Exception {
        return this.objects.value().contains(ObjectsIndex.stripped(name));
    }

    /**
     * Names of all objects located directly inside the given package, e.g.
     * {@code "tuple.each"} and {@code "tuple.eachi"} for {@code "tuple"}, but
     * not {@code "tuple"} itself nor objects from its sub-packages. The
     * bare root package (stripped to an empty string) works the same way:
     * its direct children are the top-level names with no dot at all, e.g.
     * {@code "tuple"} and {@code "math"} themselves.
     * @param pkg Package name, e.g. {@code "tuple"}, {@code "math.vector"}
     *  or {@code "org.eolang"} itself
     * @return Object names that live directly in that package
     * @throws Exception If the index can't be read
     */
    Iterable<String> children(final String pkg) throws Exception {
        final String stripped = ObjectsIndex.stripped(pkg);
        final String prefix;
        if (stripped.isEmpty()) {
            prefix = stripped;
        } else {
            prefix = String.format("%s.", stripped);
        }
        return new Filtered<>(
            name -> name.startsWith(prefix) && name.indexOf('.', prefix.length()) < 0,
            this.objects.value()
        );
    }

    private static String stripped(final String name) {
        final String root = "org.eolang";
        final String prefix = String.format("%s.", root);
        final String result;
        if (name.equals(root)) {
            result = "";
        } else if (name.startsWith(prefix)) {
            result = name.substring(prefix.length());
        } else {
            result = name;
        }
        return result;
    }

    private static String convert(final String name) {
        return name.substring(0, name.length() - 3)
            .replace('/', '.')
            .substring(name.indexOf('/') + 1);
    }

    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private static Text asText(final URL url) throws Exception {
        return new TextOf(new TextOf(url).asString());
    }
}
