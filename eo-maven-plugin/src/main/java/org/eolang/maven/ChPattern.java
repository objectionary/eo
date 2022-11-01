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

/**
 * Commit Hash pattern.
 *
 * Returns hash values by particular pattern:
 * -DofflineHash=0.*.*:abc2sd3
 * -DofflineHash=0.2.7:abc2sd3,0.2.8:s4se2fe
 *
 * @since 0.28.11
 */
final class ChPattern implements CommitHash {

    /**
     * Pattern like *.*.* or 'master'.
     */
    private final String pattern;

    /**
     * Particular tag to match.
     */
    private final String tag;

    /**
     * The main constructor.
     *
     * @param pattern Pattern like *.*.* or 'master'.
     * @param tag Particular tag to match.
     */
    ChPattern(
        final String pattern,
        final String tag
    ) {
        this.pattern = pattern;
        this.tag = tag;
    }

    @Override
    public String value() {
        return this.pattern + this.tag;
    }
}
