/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
 * Named attribute.
 *
 * @since 0.1
 */
public final class AtNamed implements Attr {

    private final Attr origin;

    private final Phi phi;

    private final String name;

    private final String oname;

    public AtNamed(final String nme, final String onme,
        final Phi src, final Attr attr) {
        this.name = nme;
        this.oname = onme;
        this.phi = src;
        this.origin = attr;
    }

    @Override
    public String toString() {
        return String.format("%sN", this.origin);
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    @Override
    public Attr copy(final Phi self) {
        try {
            return new AtNamed(this.name, this.oname, this.phi, this.origin.copy(self));
        } catch (final IllegalAttrException ex) {
            throw new IllegalAttrException(this.label(), ex);
        }
    }

    @Override
    public Phi get() {
        Phi obj;
        try {
            obj = this.origin.get();
        } catch (final Attr.StillAbstractException ex) {
            throw new Attr.StillAbstractException(this.label(), ex);
        } catch (final IllegalAttrException ex) {
            throw new IllegalAttrException(this.label(), ex);
        }
        if (!(obj instanceof Data)) {
            obj = new PhNamed(obj, this.oname);
        }
        return obj;
    }

    @Override
    public void put(final Phi src) {
        try {
            this.origin.put(src);
        } catch (final Attr.ReadOnlyException ex) {
            throw new Attr.ReadOnlyException(this.label(), ex);
        } catch (final IllegalAttrException ex) {
            throw new IllegalAttrException(this.label(), ex);
        }
    }

    /**
     * The label of the exception.
     * @return Label
     */
    private String label() {
        return String.format("Error at \"%s\" attribute", this.name);
    }

}
