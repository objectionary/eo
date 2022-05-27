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

package EOorg.EOeolang.EOgray;

import org.eolang.Attr;
import org.eolang.ExFailure;
import org.eolang.Phi;

/**
 * CAGE attribute.
 *
 * @since 0.19
 */
final class AtCage implements Attr {

    /**
     * The term to show when empty.
     */
    public static final String EMPTY_TERM = "Ø";

    /**
     * The object being caged.
     */
    private Phi object;

    /**
     * Ctor.
     *
     * It sets the encapsulated object to NULL, which means
     * that there is nothing yet in the cage.
     */
    AtCage() {
        this(null);
    }

    /**
     * Ctor, needed for copying.
     * @param obj New object
     */
    private AtCage(final Phi obj) {
        this.object = obj;
    }

    @Override
    public Attr copy(final Phi self) {
        return new AtCage(this.object);
    }

    @Override
    public Phi get() {
        if (this.object == null) {
            throw new ExFailure(
                "The cage is empty, can't read it"
            );
        }
        return this.object;
    }

    @Override
    public void put(final Phi phi) {
        this.object = phi;
    }

    @Override
    public String φTerm() {
        final String txt;
        if (this.object == null) {
            txt = AtCage.EMPTY_TERM;
        } else {
            txt = this.object.φTerm();
        }
        return txt;
    }

    @Override
    public String toString() {
        final String txt;
        if (this.object == null) {
            txt = "NULL";
        } else {
            txt = this.object.toString();
        }
        return String.format("%d->%s", this.hashCode(), txt);
    }

}
