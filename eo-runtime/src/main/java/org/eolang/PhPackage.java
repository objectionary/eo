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

import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A package object, coming from {@link Phi}.
 *
 * @since 0.22
 */
@Versionized
final class PhPackage implements Phi {

    /**
     * The name of the Java package.
     */
    private final String pkg;

    /**
     * All of them.
     */
    private final ThreadLocal<Map<String, Phi>> objects = ThreadLocal.withInitial(
        () -> new ConcurrentHashMap<>(0)
    );

    /**
     * Ctor.
     * @param name The name
     */
    PhPackage(final String name) {
        this.pkg = name;
    }

    @Override
    public Attr attr(final String name) {
        final String obj = this.eoPackage(name);
        final String key = new JavaPath(obj).toString();
        if (!this.objects.get().containsKey(key)) {
            this.objects.get().put(key, this.loadPhi(key).orElseGet(() -> new PhPackage(obj)));
        }
        return new AtSimple(this.objects.get().get(key));
    }

    @Override
    public String locator() {
        return "?:?";
    }

    @Override
    public String forma() {
        throw new ExFailure(
            String.format("Can't #type() from package object '%s'", this.pkg)
        );
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
     * Creates eo-package path by name.
     * @param name The name of an en object.
     * @return Eo-package path.
     */
    private String eoPackage(final String name) {
        final StringBuilder abs = new StringBuilder(0).append(this.pkg);
        if (abs.length() > 0) {
            abs.append('.');
        }
        abs.append(name);
        return abs.toString();
    }

    /**
     * Load phi object by package name from ClassLoader.
     * @param target The package name
     * @return Phi
     */
    private Optional<Phi> loadPhi(final String target) {
        Optional<Phi> res;
        try {
            final Phi kid = (Phi) Class.forName(target)
                .getConstructor(Phi.class)
                .newInstance(Phi.Φ);
            kid.attr("ρ").put(this);
            res = Optional.of(kid);
        } catch (final ClassNotFoundException notfound) {
            res = Optional.empty();
        } catch (final NoSuchMethodException
            | InvocationTargetException
            | InstantiationException
            | IllegalAccessException ex
        ) {
            throw new ExFailure(
                String.format(
                    "Can't find Java object/package '%s' in EO package '%s'",
                    target, this.pkg
                ),
                ex
            );
        }
        return res;
    }
}
