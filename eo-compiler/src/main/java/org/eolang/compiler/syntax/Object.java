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

import java.util.Collection;
import org.eolang.compiler.java.JavaClass;
import org.eolang.compiler.java.JavaFile;

/**
 * EO Object.
 *
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class Object implements RootNode {

    /**
     * Object name.
     *
     * @todo #95:30m Object name should be optional.
     *  As described in #54, object can be anonymous.
     *  I think we should generate some java class name in this case.
     */
    private final String name;

    /**
     * Object types.
     */
    private final Collection<String> types;

    /**
     * Object body.
     */
    private final ObjectBody body;

    /**
     * Ctor.
     *
     * @param name Object name
     * @param types Object types
     * @param body Object body
     */
    public Object(
        final String name,
        final Collection<String> types,
        final ObjectBody body
    ) {
        this.name = name;
        this.types = types;
        this.body = body;
    }

    @Override
    public JavaFile java() {
        return new JavaClass(
            this.name,
            this.types,
            this.body
        );
    }
}
