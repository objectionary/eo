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

import org.cactoos.iterable.Mapped;
import org.cactoos.text.FormattedText;
import org.cactoos.text.JoinedText;
import org.cactoos.text.UncheckedText;
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
    private final Iterable<Attribute> attributes;

    /**
     * Ctor.
     *
     * @param ctor Class name.
     * @param attrs Object attributes.
     */
    public PrimaryConstructor(final String ctor,
        final Iterable<Attribute> attrs) {
        this.attributes = attrs;
        this.name = ctor;
    }

    /**
     * Generate constructor java code.
     *
     * @return Java code for constructor.
     */
    public String code() {
        return new UncheckedText(
            new FormattedText(
                "public %s(%s) {\n %s \n}",
                this.name,
                new UncheckedText(
                    new JoinedText(
                        ", ",
                        new Mapped<>(
                            attr -> attr.java(
                                PrimaryConstructor.CTOR_PARAM_FORMAT
                            ),
                            this.attributes
                        )
                    )
                ).asString(),
                new UncheckedText(
                    new JoinedText(
                        "\n",
                        new Mapped<>(
                            attr -> attr.java(
                                PrimaryConstructor.CTOR_INIT_FORMAT
                            ),
                            this.attributes
                        )
                    )
                ).asString()
            )
        ).asString();
    }
}
