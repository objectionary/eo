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

package org.eolang;

import java.lang.reflect.InvocationTargetException;

/**
 * A package object, coming from {@link Phi}.
 *
 * @since 0.22
 */
final class PhPackage implements Phi {

    /**
     * The name of the Java package.
     */
    private final String pkg;

    /**
     * Ctor.
     * @param name The name
     */
    PhPackage(final String name) {
        this.pkg = name;
    }

    @Override
    public Attr attr(final String name) {
        final String abs = String.format("%s.%s", this.pkg, name);
        final String target = abs.replaceAll("(^|\\.)([^.]+)", "$1EO$2");
        try {
            Class.forName(String.format("%s.package-info", target));
            return new AtSimple(new PhPackage(abs));
        } catch (final ClassNotFoundException ex) {
            // ignore it
            System.out.println("failed: " + target);
        }
        try {
            return new AtSimple(
                Phi.class.cast(
                    Class.forName(target).getConstructor(Phi.class).newInstance(Phi.Φ)
                )
            );
        } catch (final NoSuchMethodException | ClassNotFoundException
            | InvocationTargetException | InstantiationException
            | IllegalAccessException ex) {
            throw new ExFailure(
                String.format(
                    "Can't find EO object '%s' in Java package '%s' by the name '%s'",
                    name, this.pkg, target
                ),
                ex
            );
        }
    }

    @Override
    public String φTerm() {
        throw new UnsupportedOperationException(
            String.format("Φ.%s", this.pkg)
        );
    }

    @Override
    public Attr attr(final int pos) {
        throw new ExFailure(
            String.format("Can't #attr(%d) from package object '%s'", pos, this.pkg)
        );
    }

    @Override
    public Phi copy() {
        throw new ExFailure(
            String.format("Can't #copy() package object '%s'", this.pkg)
        );
    }

    @Override
    public void move(final Phi rho) {
        throw new ExFailure(
            String.format("Can't #move() package object '%s'", this.pkg)
        );
    }

}
