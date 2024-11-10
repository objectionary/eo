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
public final class PhNamed implements Phi, Atom {

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
        return this.through(() -> this.origin.equals(obj));
    }

    @Override
    public int hashCode() {
        return this.through(this.origin::hashCode);
    }

    @Override
    public String toString() {
        return this.through(() -> String.format("%s≡%s", this.name, this.origin.toString()));
    }

    @Override
    public String φTerm() {
        return this.through(() -> String.format("%s ≡ %s", this.name, this.origin.φTerm()));
    }

    @Override
    public Phi copy() {
        return this.through(() -> new PhNamed(this.origin.copy(), this.name));
    }

    @Override
    public Phi take(final String sub) {
        return this.through(
            () -> new PhNamed(
                this.origin.take(sub),
                String.format("%s.%s", this.name, sub)
            )
        );
    }

    @Override
    public boolean put(final int pos, final Phi object) {
        return this.through(() -> this.origin.put(pos, object));
    }

    @Override
    public boolean put(final String nme, final Phi object) {
        return this.through(() -> this.origin.put(nme, object));
    }

    @Override
    public String locator() {
        return this.through(this.origin::locator);
    }

    @Override
    public String forma() {
        return this.through(this.origin::forma);
    }

    @Override
    public void attach(final byte[] data) {
        this.through(
            () -> {
                this.origin.attach(data);
                return null;
            }
        );
    }

    @Override
    public byte[] delta() {
        return this.through(this.origin::delta, ".Δ");
    }

    @Override
    public Phi lambda() {
        return this.through(() -> ((Atom) this.origin).lambda(), ".λ");
    }

    /**
     * Helper, for other methods.
     * @param action The action
     * @param <T> Type of result
     * @return Result
     */
    private <T> T through(final Action<T> action) {
        return this.through(action, "");
    }

    /**
     * Helper, for other methods.
     * @param action The action
     * @param suffix The suffix to add to the label
     * @param <T> Type of result
     * @return Result
     */
    private <T> T through(final Action<T> action, final String suffix) {
        try {
            return action.act();
        } catch (final ExUnset ex) {
            throw new ExUnset(this.label(suffix), ex);
        } catch (final ExReadOnly ex) {
            throw new ExReadOnly(this.label(suffix), ex);
        } catch (final ExAbstract ex) {
            throw new ExFailure(this.label(suffix), ex);
        }
    }

    /**
     * The label of the exception.
     * @param suffix The suffix to add to the label
     * @return Label
     */
    private String label(final String suffix) {
        return String.format("Error at \"%s%s\" attribute", this.name, suffix);
    }
}
