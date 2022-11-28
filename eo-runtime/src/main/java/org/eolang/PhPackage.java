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

package org.eolang;

import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

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
     * All of them.
     */
    private final Map<String, Phi> objects =
        new ConcurrentHashMap<>(0);

    /**
     * Ctor.
     * @param name The name
     */
    PhPackage(final String name) {
        this.pkg = name;
    }

    @Override
    public Attr attr(final String name) {
        final StringBuilder abs = new StringBuilder(0).append(this.pkg);
        if (abs.length() > 0) {
            abs.append('.');
        }
        abs.append(name);
        final String target = abs.toString()
            .replaceAll("(^|\\.)([^.]+)", "$1EO$2")
            .replace("$", "$EO")
            .replace("-", "_");
        return new AtSimple(
            this.objects.computeIfAbsent(
                target, t -> {
                    Phi phi;
                    try {
                        phi = this.sub(t);
                    } catch (final ClassNotFoundException ex) {
                        phi = new PhPackage(abs.toString());
                    }
                    return phi;
                }
            )
        );
    }

    @Override
    public String locator() {
        return "?:?";
    }

    @Override
    public String toString() {
        return String.format("Φ.%s", this.pkg);
    }

    @Override
    public String φTerm() {
        return String.format("Φ.%s", this.pkg);
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

    /**
     * Make a sub package.
     * @param target The name
     * @return Phi
     * @throws ClassNotFoundException If not found
     */
    private Phi sub(final String target) throws ClassNotFoundException {
        try {
            final Phi kid = Phi.class.cast(
                Class.forName(target).getConstructor(Phi.class).newInstance(Phi.Φ)
            );
            kid.attr("ρ").put(this);
            return kid;
        } catch (final NoSuchMethodException
            | InvocationTargetException | InstantiationException
            | IllegalAccessException ex) {
            throw new ExFailure(
                String.format(
                    "Can't find Java object/package '%s' in EO package '%s'",
                    target, this.pkg
                ),
                ex
            );
        }
    }
}
