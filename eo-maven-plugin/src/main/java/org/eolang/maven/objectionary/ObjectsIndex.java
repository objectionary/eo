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

import java.net.URL;
import java.util.Set;
import org.cactoos.Scalar;
import org.cactoos.Text;
import org.cactoos.iterable.Mapped;
import org.cactoos.scalar.ScalarOf;
import org.cactoos.scalar.Sticky;
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
     * Cached objects index.
     */
    private final Scalar<? extends Set<String>> objects;

    /**
     * Ctor.
     */
    ObjectsIndex() {
        this(
            new ScalarOf<Set<String>>(
                () -> new SetOf<>(
                    new Mapped<>(
                        ObjectsIndex::convert,
                        new Mapped<>(
                            Text::asString,
                            new Split(
                                new TextOf(
                                    new URL("https://home.objectionary.com/objectionary.lst")
                                ),
                                "\n"
                            )
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
}
