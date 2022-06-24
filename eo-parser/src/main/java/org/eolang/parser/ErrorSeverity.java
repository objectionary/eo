/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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

import java.util.Objects;

/**
 * Parsing errors severity.
 * @since 1.0
 */
public interface ErrorSeverity {
    /**
     * Warning instance.
     */
    ErrorSeverity WARNING = new EsBase("warning");
    /**
     * Error instance.
     */
    ErrorSeverity ERROR = new EsBase("error");
    /**
     * Advice instance.
     */
    ErrorSeverity ADVICE = new EsBase("advice");

    /**
     * Severity text.
     * @return String representation of severity value
     */
    String asText();

    /**
     * Base implementation of {@link ErrorSeverity}.
     * @since 1.0
     */
    class EsBase implements ErrorSeverity {
        /**
         * Severity text.
         */
        private final String text;

        /**
         * Ctor.
         * @param text Severity text
         */
        public EsBase(final String text) {
            this.text = text;
        }

        @Override
        public String asText() {
            return this.text;
        }

        @Override
        public boolean equals(final Object other) {
            if (this == other) {
                return true;
            }
            if (other == null || getClass() != other.getClass()) {
                return false;
            }
            final EsBase base = (EsBase) other;
            return Objects.equals(this.text, base.text);
        }

        @Override
        public int hashCode() {
            return Objects.hash(this.text);
        }
    }
}
