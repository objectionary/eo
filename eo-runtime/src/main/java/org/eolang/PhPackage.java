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
import java.util.Arrays;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A package object, coming from {@link Phi}.
 * @since 0.22
 */
@Versionized
@SuppressWarnings("PMD.TooManyMethods")
final class PhPackage implements Phi {

    /**
     * The name of the Java package.
     */
    private final String pkg;

    /**
     * All of them.
     */
    private final ThreadLocal<Map<String, Phi>> objects;

    /**
     * Ctor.
     * @param name The name
     */
    PhPackage(final String name) {
        this.pkg = name;
        this.objects = ThreadLocal.withInitial(
            () -> new ConcurrentHashMap<>(0)
        );
    }

    @Override
    public String locator() {
        return "?:?";
    }

    @Override
    public String forma() {
        throw new ExFailure(
            String.format("Can't #form() from package object '%s'", this.pkg)
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
    public Phi copy() {
        throw new ExFailure(
            String.format("Can't #copy() package object '%s'", this.pkg)
        );
    }

    @Override
    public Phi take(final String name) {
        final String obj = this.eoPackage(name);
        final String key = new JavaPath(obj).toString();
        if (!this.objects.get().containsKey(key)) {
            this.objects.get().put(key, this.loadPhi(key).orElseGet(() -> new PhPackage(obj)));
        }
        final Phi res;
        final Phi phi = this.objects.get().get(key);
        if (phi instanceof PhPackage) {
            res = phi;
        } else {
            res = new AtSetRho(this.objects.get().get(key), this, key).get();
        }
        this.objects.remove();
        return res;
    }

    @Override
    public boolean put(final int pos, final Phi object) {
        throw new IllegalStateException(
            String.format("Can't #put(%d, %s) to package object '%s'", pos, object, this.pkg)
        );
    }

    @Override
    public boolean put(final String name, final Phi object) {
        throw new IllegalStateException(
            String.format("Can't #put(%s, %s) to package object '%s'", name, object, this.pkg)
        );
    }

    @Override
    public void attach(final byte[] data) {
        throw new IllegalStateException(
            String.format(
                "Can't #attac(%s) to package object '%s'", Arrays.toString(data), this.pkg
            )
        );
    }

    @Override
    public byte[] delta() {
        throw new IllegalStateException(
            String.format("Can't #data() from package object '%s'", this.pkg)
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
            res = Optional.of(
                (Phi) Class.forName(target)
                    .getConstructor()
                    .newInstance()
            );
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
