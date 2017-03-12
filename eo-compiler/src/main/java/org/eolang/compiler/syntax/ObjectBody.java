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
import java.util.stream.Collectors;
import org.eolang.compiler.java.PrimaryConstructor;

/**
 * Object body.
 *
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class ObjectBody {

    /**
     * Attribute field format.
     */
    private static final AttributeFormat FIELD_FORMAT = new AttrFieldFormat();

    /**
     * Object attributes.
     */
    private final Collection<Attribute> attrs;

    /**
     * Secondary constructors.
     */
    private final Collection<Ctor> ctors;

    /**
     * Ctor.
     *
     * @param attrs Object attributes
     * @param ctors Object secondary constructors
     */
    public ObjectBody(
        final Collection<Attribute> attrs,
        final Collection<Ctor> ctors
    ) {
        this.attrs = attrs;
        this.ctors = ctors;
    }

    /**
     * Java code for object body.
     *
     * @param name Java class name
     * @return Java code
     */
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    public String java(final String name) {
        return String.format(
            String.join(
                "\n",
                "// fields",
                "%s\n",
                "// secondary constructors",
                "%s\n",
                "// primary constructor",
                "%s"
            ),
            String.join(
                "\n",
                this.attrs
                    .stream()
                    .map(attr -> attr.java(ObjectBody.FIELD_FORMAT))
                    .collect(Collectors.toList())
            ),
            String.join(
                "\n",
                this.ctors
                    .stream()
                    .map(ctor -> ctor.java(name))
                    .collect(Collectors.toList())
            ),
            new PrimaryConstructor(name, this.attrs).code()
        );
    }
}
