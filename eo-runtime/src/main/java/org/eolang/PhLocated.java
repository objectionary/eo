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

import EOorg.EOeolang.EOerror;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

/**
 * An object with coordinates (line and position).
 *
 * @since 0.21
 */
@SuppressWarnings("PMD.TooManyMethods")
@Versionized
public final class PhLocated implements Phi, Atom {

    /**
     * The original.
     */
    private final Phi origin;

    /**
     * The line number.
     */
    private final int line;

    /**
     * The position in the line.
     */
    private final int position;

    /**
     * The location.
     */
    private final String location;

    /**
     * Ctor.
     *
     * @param phi The object
     * @param lne Line
     * @param pos Position
     */
    public PhLocated(final Phi phi, final int lne, final int pos) {
        this(phi, lne, pos, "?");
    }

    /**
     * Ctor.
     *
     * @param phi The object
     * @param lne Line
     * @param pos Position
     * @param loc Location
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    public PhLocated(final Phi phi, final int lne, final int pos, final String loc) {
        this.origin = phi;
        this.line = lne;
        this.position = pos;
        this.location = loc;
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
        return String.format(
            "<%s>%s",
            this.locator(),
            this.origin.toString()
        );
    }

    @Override
    public String φTerm() {
        return this.origin.φTerm();
    }

    @Override
    public Phi copy() {
        return new PhLocated(this.origin.copy(), this.line, this.position, this.location);
    }

    @Override
    public Phi take(final String name) {
        return this.through(() -> this.origin.take(name));
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
        return String.format("%s:%d:%d", this.location, this.line, this.position);
    }

    @Override
    public String forma() {
        return this.through(this.origin::forma);
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
     * @checkstyle IllegalCatchCheck (20 lines)
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    private <T> T through(final Action<T> action, final String suffix) {
        try {
            return action.act();
        } catch (final ExUnset ex) {
            throw new ExUnset(this.label(suffix), ex);
        } catch (final ExReadOnly ex) {
            throw new ExReadOnly(this.label(suffix), ex);
        } catch (final EOerror.ExError ex) {
            throw new EOerror.ExError(ex, this.label(suffix));
        } catch (final ExAbstract | Error ex) {
            throw new ExFailure(this.label(suffix), ex);
        } catch (final RuntimeException ex) {
            throw this.wrap(ex, suffix);
        }
    }

    /**
     * Wrap the exception into a new one.
     * @param cause Original
     * @param suffix Te suffix
     * @return New exception
     */
    private RuntimeException wrap(final RuntimeException cause,
        final String suffix) {
        final String label = this.label(suffix);
        RuntimeException ret;
        try {
            final Constructor<? extends RuntimeException> ctor =
                cause.getClass().getConstructor(String.class, Throwable.class);
            ret = ctor.newInstance(label, cause);
        } catch (final NoSuchMethodException | InstantiationException
            | IllegalAccessException | InvocationTargetException ex) {
            ret = new ExFailure(label, cause);
        }
        return ret;
    }

    /**
     * The label of the exception.
     * @param suffix The suffix to add to the label
     * @return Label
     */
    private String label(final String suffix) {
        return String.format(
            "Error in the \"%s%s\" attribute at %s:%s",
            this.location, suffix, this.line, this.position
        );
    }
}
