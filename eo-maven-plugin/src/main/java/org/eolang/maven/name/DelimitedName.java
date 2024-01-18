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
package org.eolang.maven.name;

import java.util.Optional;
import java.util.regex.Pattern;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;

/**
 * A class which encapsulates delimiter.
 * It parses a raw string into two parts: title and optional label,
 * dividing by means of the delimiter.
 * It also does a reverse operation, concatenation of two strings
 * with the delimiter (method {@link #toString()}).
 * <p>
 * E.g. it divides a raw (concat) string as:
 * - "org.eolang.text|0.1.0" into "org.eolang.text" and "0.1.0"
 * - "org.eolang.text" into "org.eolang.text" and empty {@link Optional}
 * <p>
 * It joins:
 * - title "org.eolang.text" and label "0.1.0" into "org.eolang.text|0.1.0"
 * - title "org.eolang.text" and an empty {@link Optional} label into "org.eolang.text"
 * @since 0.33
 */
public final class DelimitedName {
    /**
     * Delimiter between title and label.
     */
    private static final String DELIMITER = "|";

    /**
     * Pre-compiled regex form of {@link #DELIMITER}.
     */
    private static final Pattern DELIMITER_REGEX = Pattern.compile(
        String.format(
            "\\%s",
            DelimitedName.DELIMITER
        )
    );

    /**
     * The state of an object - a pair of title and label.
     */
    private final Unchecked<String[]> pair;

    /**
     * Ctor.
     * @param concat Raw string.
     */
    public DelimitedName(final String concat) {
        this(() -> concat);
    }

    /**
     * Ctor.
     * @param concat Raw string as scalar.
     */
    public DelimitedName(final Scalar<String> concat) {
        this(
            new Unchecked<>(
                () -> DELIMITER_REGEX.split(concat.value(), 2)
            )
        );
    }

    /**
     * Ctor.
     * @param title The title.
     * @param label The optional label
     */
    public DelimitedName(final String title, final String label) {
        this(title, Optional.of(label));
    }

    /**
     * Ctor.
     * @param title The title.
     * @param label The optional label.
     */
    public DelimitedName(final String title, final Optional<String> label) {
        this(
            new Unchecked<>(
                () -> label.map(unwrap -> new String[]{title, unwrap})
                    .orElseGet(() -> new String[]{title})
            )
        );
    }

    /**
     * Ctor.
     * @param pair A pair of (title, label) as array of dimension 2 or 1 (if no label).
     */
    private DelimitedName(final Unchecked<String[]> pair) {
        this.pair = new Unchecked<>(new Sticky<>(pair));
    }

    /**
     * The title.
     * @return The title.
     */
    public String title() {
        return this.pair.value()[0];
    }

    /**
     * The optional label. Label can be absent.
     * @return The label.
     */
    public Optional<String> label() {
        final Optional<String> res;
        final String[] unwrap = this.pair.value();
        if (unwrap.length > 1) {
            res = Optional.of(unwrap[1]);
        } else {
            res = Optional.empty();
        }
        return res;
    }

    @Override
    public String toString() {
        return String.join(DelimitedName.DELIMITER, this.pair.value());
    }
}
