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

package org.eolang;

/**
 * Located attribute.
 *
 * @since 0.21
 */
@Versionized
final class AtLocated implements Attr {

    /**
     * Original attribute.
     */
    private final Attr origin;

    /**
     * The line number.
     */
    private final int line;

    /**
     * The position in the line.
     */
    private final int position;

    /**
     * The location of the program.
     */
    private final String location;

    /**
     * Ctor.
     * @param attr Original
     * @param lne Line
     * @param pos Position
     */
    AtLocated(final Attr attr, final int lne, final int pos) {
        this(attr, lne, pos, "?");
    }

    /**
     * Ctor.
     * @param attr Original
     * @param lne Line
     * @param pos Position
     * @param loc Location
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    AtLocated(final Attr attr, final int lne, final int pos, final String loc) {
        this.origin = attr;
        this.line = lne;
        this.position = pos;
        this.location = loc;
    }

    @Override
    public String toString() {
        return String.format(
            "<%s:%d:%d>%s",
            this.location,
            this.line,
            this.position,
            this.origin.toString()
        );
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtLocated(this.origin.copy(self), this.line, this.position);
    }

    @Override
    public Phi get() {
        final Phi obj;
        try {
            obj = this.origin.get();
        } catch (final ExFlow ex) {
            throw ex;
        } catch (final ExAbstract ex) {
            throw new ExFailure(this.label(), ex);
        }
        return obj;
    }

    @Override
    public void put(final Phi src) {
        try {
            this.origin.put(src);
        } catch (final ExFlow ex) {
            throw ex;
        } catch (final ExAbstract ex) {
            throw new ExFailure(this.label(), ex);
        }
    }

    /**
     * The label of the exception.
     * @return Label
     */
    private String label() {
        return String.format(
            "The object is at the line #%d, position #%d within `%s`",
            this.line,
            this.position,
            this.location
        );
    }

}
