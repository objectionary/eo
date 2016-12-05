/**
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 eolang.org
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
package org.eolang.compiler.syntax;

import com.google.common.base.Joiner;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.AbstractMap;
import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Type.
 *
 * @author Yegor Bugayenko (yegor256@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class Type {

    /**
     * Text to parse.
     */
    private final String name;

    /**
     * Methods.
     */
    private final Collection<Method> methods;

    /**
     * Ctor.
     * @param label Type name
     * @param mts Methods
     */
    public Type(final String label, final Collection<Method> mts) {
        this.name = label;
        this.methods = mts;
    }

    /**
     * Convert it to Java file (path, content).
     * @return Java code
     */
    public Map.Entry<Path, String> java() {
        return new AbstractMap.SimpleEntry<>(
            Paths.get(String.format("%s.java", this.name)),
            String.format(
                "public interface %s {\n    %s\n}",
                this.name,
                Joiner.on("\n    ").join(
                    this.methods.stream().map(
                        Method::java
                    ).collect(Collectors.toList())
                )
            )
        );
    }

}
