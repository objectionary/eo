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
package org.eolang.compiler.java;

import java.util.Collection;
import java.util.stream.Collectors;
import org.eolang.compiler.syntax.AttrCtorInitFormat;
import org.eolang.compiler.syntax.AttrCtorParamFormat;
import org.eolang.compiler.syntax.Attribute;
import org.eolang.compiler.syntax.AttributeFormat;

/**
 * Java class primary constructor.
 *
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class PrimaryConstructor {

    /**
     * Attribute constructor parameter format.
     */
    private static final AttributeFormat CTOR_PARAM_FORMAT =
        new AttrCtorParamFormat();

    /**
     * Attribute constructor initializer format.
     */
    private static final AttributeFormat CTOR_INIT_FORMAT =
        new AttrCtorInitFormat();

    /**
     * Java class name.
     */
    private final String name;

    /**
     * Object attributes.
     */
    private final Collection<Attribute> attributes;

    /**
     * Ctor.
     *
     * @param name Class name.
     * @param attributes Object attributes.
     */
    public PrimaryConstructor(
        final String name,
        final Collection<Attribute> attributes
    ) {
        this.attributes = attributes;
        this.name = name;
    }

    /**
     * Generate constructor java code.
     *
     * @return Java code for constructor.
     */
    public String code() {
        return String.format(
            "public %s(%s) {\n %s \n}",
            this.name,
            String.join(
                ", ",
                this.attributes
                    .stream()
                    .map(
                        attr -> attr.java(
                            PrimaryConstructor.CTOR_PARAM_FORMAT
                        )
                    )
                    .collect(Collectors.toList())
            ),
            String.join(
                "\n",
                this.attributes
                    .stream()
                    .map(
                        attr -> attr.java(
                            PrimaryConstructor.CTOR_INIT_FORMAT
                        )
                    )
                    .collect(Collectors.toList())
            )
        );
    }
}
