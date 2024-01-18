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
package org.eolang.maven.hash;

import java.util.Map;
import java.util.regex.Pattern;
import org.cactoos.Scalar;
import org.cactoos.iterable.Mapped;
import org.cactoos.map.MapEntry;
import org.cactoos.map.MapEnvelope;
import org.cactoos.map.MapOf;
import org.cactoos.text.Split;

/**
 * Commit hashes table as a map.
 * The keys - tags
 * The values - narrow hashes (7 chars)
 *
 * @since 0.29.6
 */
public final class CommitHashesMap extends MapEnvelope<String, CommitHash> {

    /**
     * Whitespace pattern.
     */
    private static final Pattern WHITESPACE = Pattern.compile("\\s+");

    /**
     * Constructor.
     */
    public CommitHashesMap() {
        this(new CommitHashesText()::asString);
    }

    /**
     * Ctor.
     * @param table Commit hashes table as string.
     */
    private CommitHashesMap(final String table) {
        this(() -> table);
    }

    /**
     * Ctor.
     * @param table Commit hashes table.
     */
    private CommitHashesMap(final Scalar<String> table) {
        super(CommitHashesMap.fromTable(table));
    }

    /**
     * Prestructor from hashes table.
     * You can read more about prestructors and why they are needed right
     * <a href="https://www.yegor256.com/2021/08/04/prestructors.html">here</a>
     * @param table Commit hashes table as string value.
     * @return Map of commit hashes.
     */
    private static Map<String, CommitHash> fromTable(final Scalar<String> table) {
        return new MapOf<>(
            new Mapped<>(
                rows -> {
                    final String[] row = CommitHashesMap.WHITESPACE.split(rows.asString());
                    return new MapEntry<>(
                        row[1],
                        new ChCached(
                            new ChNarrow(
                                new CommitHash.ChConstant(row[0])
                            )
                        )
                    );
                },
                new Split(table::value, "\n")
            )
        );
    }

    /**
     * Fake commit hashes hash-table.
     *
     * @since 0.29.6
     */
    public static final class Fake extends MapEnvelope<String, CommitHash> {
        /**
         * Ctor.
         */
        public Fake() {
            super(
                new CommitHashesMap(
                    String.join(
                        "\n",
                        "5fe5ad8d21dbe418038fa4c86e096fb037f290a9 0.23.15",
                        "15c85d7f8cffe15b0deba96e90bdac98a76293bb 0.23.17",
                        "4b19944d86058e3c81e558340a3a13bc335a2b48 0.23.19",
                        "0aa6875c40d099c3f670e93d4134b629566c5643 0.25.0",
                        "ff32e9ff70c2b3be75982757f4b0607dc37b258a 0.25.5",
                        "e0b783692ef749bb184244acb2401f551388a328 0.26.0",
                        "cc554ab82909eebbfdacd8a840f9cf42a99b64cf 0.27.0",
                        "00b60c7b2112cbad4e37ba96b162469a0e75f6df 0.27.2",
                        "6a70071580e95aeac104b2e48293d3dfe0669973 0.28.0",
                        "0c15066a2026cec69d613b709a301f1573f138ec 0.28.1",
                        "9b883935257bd59d1ba36240f7e213d4890df7ca 0.28.10",
                        "a7a4556bf1aa697324d805570f42d70affdddb75 0.28.14",
                        "54d83d4b1d28075ee623d58fd742eaa529febd3d 0.28.2",
                        "6c6269d1f9a1c81ffe641538f119fe4e12706cb3 0.28.4",
                        "9c9352890b5d30e1b89c9147e7c95a90c9b8709f 0.28.5",
                        "17f89293e5ae6115e9a0234b754b22918c11c602 0.28.6",
                        "5f82cc1edffad67bf4ba816610191403eb18af5d 0.28.7",
                        "be83d9adda4b7c9e670e625fe951c80f3ead4177 0.28.9"
                    )
                )
            );
        }
    }
}
