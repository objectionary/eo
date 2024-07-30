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
/*
 * @checkstyle PackageNameCheck (4 lines)
 */
package EOorg.EOeolang.EOsys;

import com.sun.jna.Library;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import org.eolang.Dataized;
import org.eolang.Phi;

/**
 * Default native call that uses {@link Library} and can be dispatched by name.
 *
 * @since 0.39
 */
public final class DispatchedNativeDefault implements DispatchedNativeMethod {
    /**
     * Native library.
     */
    private final Library lib;

    /**
     * Dispatched method.
     */
    private final Method method;

    /**
     * Ctor.
     * @param lib Library.
     * @param method Method.
     */
    DispatchedNativeDefault(final Library lib, final Method method) {
        this.lib = lib;
        this.method = method;
    }

    /**
     * Ctor.
     * @param lib Library.
     * @param name Method name.
     */
    DispatchedNativeDefault(final Library lib, final String name) {
        this(lib, DispatchedNativeDefault.findMethodUnsafe(name, lib));
    }

    @Override
    public int call(final Phi... params) {
        try {
            return (int) this.method.invoke(this.lib, this.prepareParams(params));
        } catch (final InvocationTargetException | IllegalAccessException ex) {
            throw new IllegalStateException(
                String.format(
                    "Problem while calling syscall with name \"%s\"",
                    this.method.getName()
                ),
                ex
            );
        }
    }

    /**
     * Prepares {@link Phi} parameters to be passed to native method.
     * @param params Parameters {@link Phi}.
     * @return Prepared parameters.
     */
    private Object[] prepareParams(final Phi... params) {
        final Object[] prepared = new Object[params.length];
        final Class<?>[] types = this.method.getParameterTypes();
        for (int iter = 0; iter < params.length; iter += 1) {
            prepared[iter] = new Dataized(params[iter]).take(types[iter]);
        }
        return prepared;
    }

    /**
     * Finds method by name in native library.
     * @param name Method name.
     * @param lib Native library.
     * @return Method.
     * @throws NoSuchMethodException if method not found.
     */
    private static Method findMethod(final String name, final Library lib)
        throws NoSuchMethodException {
        for (final Method method : lib.getClass().getMethods()) {
            if (method.getName().equals(name)) {
                return method;
            }
        }
        throw new NoSuchMethodException(
            String.format(
                "Can't find syscall with name %s in class %s",
                name,
                lib.getClass().getName()
            )
        );
    }

    /**
     * Finds method by name in native library.
     * @param name Method name.
     * @param lib Native library.
     * @return Method.
     */
    private static Method findMethodUnsafe(final String name, final Library lib) {
        try {
            return DispatchedNativeDefault.findMethod(name, lib);
        } catch (final NoSuchMethodException ex) {
            throw new IllegalArgumentException(
                String.format("Can't find syscall with name \"%s\"", name),
                ex
            );
        }
    }
}
