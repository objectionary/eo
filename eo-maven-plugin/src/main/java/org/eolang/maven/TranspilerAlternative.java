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
package org.eolang.maven;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Path;
import java.util.Arrays;

/**
 * Alternative compiler.
 *
 * @since 0.1
 */
final class TranspilerAlternative implements Transpiler {

    /**
     * The name of it.
     */
    private final String name;

    /**
     * Ctor.
     * @param nam The name
     */
    TranspilerAlternative(final String nam) {
        this.name = nam;
    }

    @Override
    @SuppressWarnings("PMD.CyclomaticComplexity")
    public int transpile(final Path file, final Path generated) {
        try {
            final Class<?> clss = Class.forName(this.name);
            final Constructor<?> constructor = clss.getDeclaredConstructor(File.class);
            final Object obj = constructor.newInstance(generated.toFile());
            final Method method = Arrays
                .stream(obj.getClass().getMethods())
                .filter(mtd -> "transpile".equals(mtd.getName()))
                .findFirst()
                .get();
            method.setAccessible(true);
            method.invoke(obj, file);
            return 1;
        } catch (final ClassNotFoundException ex) {
            throw new IllegalArgumentException(
                String.format(
                    "Can't load an alternative's compiler class %s.",
                    this.name
                ),
                ex
            );
        } catch (final InvocationTargetException ex) {
            throw new IllegalArgumentException(
                String.format(
                    "Error has occurred in an alternative's compiler method(s): %s.",
                    this.name
                ),
                ex
            );
        } catch (final NoSuchMethodException ex) {
            throw new IllegalArgumentException(
                String.format(
                    "Can't load an alternative's compiler method(s) in class %s.",
                    this.name
                ),
                ex
            );
        } catch (final InstantiationException ex) {
            throw new IllegalArgumentException(
                String.format(
                    "Can't instantiate an alternative's compiler class %s.",
                    this.name
                ),
                ex
            );
        } catch (final IllegalAccessException ex) {
            throw new IllegalArgumentException(
                String.format(
                    "Not permitted to access class %s (or its method(s)) through reflection.",
                    this.name
                ),
                ex
            );
        }
    }

}
