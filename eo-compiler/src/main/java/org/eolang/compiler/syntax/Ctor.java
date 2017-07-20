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
import java.util.List;
import java.util.stream.Collectors;
import org.cactoos.text.FormattedText;
import org.cactoos.text.JoinedText;
import org.cactoos.text.UncheckedText;

/**
 * Object secondary constructor.
 *
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class Ctor {

    /**
     * Java code template.
     */
    private final String template;

    /**
     * Ctor.
     *
     * @param parameters Constructor parameters
     * @param arguments Super constructor arguments.
     */
    public Ctor(
        final List<Parameter> parameters,
        final Collection<Argument> arguments
    ) {
        this.template = new UncheckedText(
            new FormattedText(
                "public %%s(%s) {\n this(%s);\n}",
                new UncheckedText(
                    new JoinedText(
                        ", ",
                        parameters.stream()
                            .map(Parameter::java)
                            .collect(Collectors.toList())
                    )
                ).asString(),
                new UncheckedText(
                    new JoinedText(
                        ", ",
                        arguments.stream()
                            .map(Argument::java)
                            .collect(Collectors.toList())
                    )
                ).asString()
            )
        ).asString();
    }

    /**
     * Java code for constructor.
     *
     * @param name Object name
     * @return Java code
     */
    public String java(final String name) {
        return new UncheckedText(
            new FormattedText(this.template, name)
        ).asString();
    }
}
