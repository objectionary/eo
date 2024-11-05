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
 * A named object.
 *
 * @since 0.17
 */
@Versionized
@SuppressWarnings("PMD.TooManyMethods")
public final class PhNamed implements Phi {

    /**
     * The original.
     */
    private final Phi origin;

    /**
     * The name.
     */
    private final String name;

    /**
     * Ctor.
     *
     * @param phi The object
     * @param txt The name
     */
    public PhNamed(final Phi phi, final String txt) {
        this.origin = phi;
        this.name = txt;
    }

    @Override
    public boolean equals(final Object obj) {
        return this.origin.equals(obj);
    }

    @Override
    public int hashCode() {
        return this.origin.hashCode();
    }

    @Override
    public String toString() {
        return String.format("%s≡%s", this.name, this.origin.toString());
    }

    @Override
    public String φTerm() {
        return String.format("%s ≡ %s", this.name, this.origin.φTerm());
    }

    @Override
    public Phi copy() {
        try {
            return new PhNamed(this.origin.copy(), this.name);
        } catch (final ExFailure ex) {
            throw new ExFailure(this.label(), ex);
        }
    }

    @Override
    public Phi take(final String nme) {
        try {
            return new PhNamed(
                this.origin.take(nme),
                name
            );
        } catch (final ExUnset ex) {
            throw new ExUnset(this.label(), ex);
        } catch (final ExFailure ex) {
            throw new ExFailure(this.label(), ex);
        }
    }

    @Override
    public boolean put(final int pos, final Phi object) {
        try {
            return this.origin.put(pos, object);
        } catch (final ExReadOnly ex) {
            throw new ExReadOnly(this.label(), ex);
        } catch (final ExFailure ex) {
            throw new ExFailure(this.label(), ex);
        }
    }

    @Override
    public boolean put(final String nme, final Phi object) {
        try {
            return this.origin.put(nme, object);
        } catch (final ExReadOnly ex) {
            throw new ExReadOnly(this.label(), ex);
        } catch (final ExFailure ex) {
            throw new ExFailure(this.label(), ex);
        }
    }

    @Override
    public String locator() {
        return this.origin.locator();
    }

    @Override
    public String forma() {
        return this.origin.forma();
    }

    @Override
    public void attach(final byte[] data) {
        this.origin.attach(data);
    }

    @Override
    public byte[] delta() {
        return this.origin.delta();
    }

    /**
     * The label of the exception.
     * @return Label
     */
    private String label() {
        return String.format("Error at \"%s\" attribute", this.name);
    }
}
