/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2020 Yegor Bugayenko
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
import java.util.HashMap;
import java.util.Map;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.codehaus.plexus.util.ReflectionUtils;

/**
 * Mutable mojo builder.
 *
 * @since 0.1
 */
final class Mojo <T extends AbstractMojo> {

    private final Class<T> type;

    private final Map<String, Object> attrs;

    Mojo(final Class<T> tpe) {
        this.type = tpe;
        this.attrs = new HashMap<>(0);
    }

    public Mojo<T> with(final String attr, final Object value) {
        this.attrs.put(attr, value);
        return this;
    }

    public void execute() {
        try {
            final AbstractMojo mojo = this.type.newInstance();
            for (final Map.Entry<String, Object> ent : this.attrs.entrySet()) {
                final Field field =
                    ReflectionUtils.getFieldByNameIncludingSuperclasses(
                        ent.getKey(), this.type);
                if (field == null) {
                    throw new IllegalStateException(
                        String.format(
                            "Can't find \"%s\" of %s in %s",
                            ent.getKey(),
                            ent.getValue().getClass().getCanonicalName(),
                            this.type.getCanonicalName()
                        )
                    );
                }
                field.setAccessible(true);
                field.set(mojo, ent.getValue());
            }
            mojo.execute();
        } catch (MojoExecutionException | MojoFailureException | InstantiationException | IllegalAccessException e) {
            throw new IllegalStateException(e);
        }
    }

}
