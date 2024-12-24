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
package org.eolang.parser;

import java.util.List;
import java.util.Optional;
import org.cactoos.Text;
import org.cactoos.text.UncheckedText;

/**
 * The source in lines.
 * @since 0.50
 */
final class Lines {

    /**
     * The source.
     */
    private final List<Text> source;

    /**
     * Ctor.
     * @param lines The source in lines
     */
    Lines(final List<Text> lines) {
        this.source = lines;
    }

    /**
     * Get the line by number.
     * @param number The line number.
     * @return The line.
     */
    String line(final int number) {
        final Optional<String> result;
        if (number < 1 || number > this.source.size()) {
            result = Optional.empty();
        } else {
            result = Optional.ofNullable(this.source.get(number - 1))
                .map(UncheckedText::new)
                .map(UncheckedText::asString);
        }
        return result.orElse("EOF");
    }
}
