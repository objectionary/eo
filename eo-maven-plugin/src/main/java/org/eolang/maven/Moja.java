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

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;

/**
 * Mutable mojo builder.
 *
 * @param <T> Type of mojo
 * @since 0.1
 */
final class Moja<T extends AbstractMojo> {

    /**
     * The type of mojo.
     */
    private final Class<T> type;

    /**
     * All attributes.
     */
    private final Map<String, Object> attrs;

    /**
     * Ctor.
     *
     * @param tpe The type
     */
    Moja(final Class<T> tpe) {
        this.type = tpe;
        this.attrs = new HashMap<>(0);
    }

    /**
     * Add one more attribute and return self.
     *
     * @param attr The name
     * @param value The value
     * @return Itself
     */
    public Moja<T> with(final String attr, final Object value) {
        this.attrs.put(attr, value);
        return this;
    }

    /**
     * Execute it.
     */
    public void execute() {
        try {
            final AbstractMojo mojo = this.type.getConstructor().newInstance();
            for (final Map.Entry<String, Object> ent : this.attrs.entrySet()) {
                final Field field = this.field(this.type, ent.getKey());
                field.setAccessible(true);
                field.set(mojo, ent.getValue());
            }
            mojo.execute();
        } catch (final MojoExecutionException | MojoFailureException
            | InstantiationException | IllegalAccessException
            | NoSuchMethodException | InvocationTargetException ex) {
            throw new IllegalStateException(ex);
        }
    }

    /**
     * Take a field.
     * @param mojo The class
     * @param name Field name
     * @return Field
     */
    private Field field(final Class<?> mojo, final String name) {
        Field field;
        try {
            field = mojo.getDeclaredField(name);
        } catch (final NoSuchFieldException ex) {
            final Class<?> parent = mojo.getSuperclass();
            if (parent == null) {
                throw new IllegalStateException(
                    String.format(
                        "Can't find \"%s\" in %s",
                        name,
                        this.type.getCanonicalName()
                    ),
                    ex
                );
            }
            field = this.field(parent, name);
        }
        return field;
    }

}
