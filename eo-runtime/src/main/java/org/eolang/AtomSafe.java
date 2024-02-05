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
 * Atom that catches exceptions.
 * @since 0.36.0
 */
public final class AtomSafe implements Atom {
    /**
     * Original atom.
     */
    private final Atom origin;

    /**
     * Ctor.
     * @param atom Original atom.
     */
    public AtomSafe(final Atom atom) {
        this.origin = atom;
    }

    @Override
    public Phi lambda() {
        try {
            return this.origin.lambda();
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new ExInterrupted();
            // @checkstyle IllegalCatchCheck (3 line)
        } catch (final RuntimeException ex) {
            throw ex;
        } catch (final Throwable ex) {
            throw new ExFailure(
                String.format(
                    "Unexpected error '%s' of type %s",
                    ex.getMessage(),
                    ex.getClass().getSimpleName()
                ),
                ex
            );
        }
    }
}
