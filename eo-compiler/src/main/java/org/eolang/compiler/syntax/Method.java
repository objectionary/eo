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
import org.cactoos.iterable.Mapped;
import org.cactoos.text.FormattedText;
import org.cactoos.text.JoinedText;
import org.cactoos.text.UncheckedText;
import org.eolang.compiler.xml.XmlMethodDef;
import org.xembly.Directive;

/**
 * Method.
 *
 * @author Yegor Bugayenko (yegor256@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class Method {

    /**
     * Method name.
     */
    private final String name;

    /**
     * Return type name.
     */
    private final String type;

    /**
     * Parameters.
     */
    private final Collection<Parameter> parameters;

    /**
     * Ctor.
     *
     * @param mtd Method name
     * @param params Parameters
     * @param rtp Return type
     */
    public Method(final String mtd, final Collection<Parameter> params,
        final String rtp) {
        this.name = mtd;
        this.parameters = params;
        this.type = rtp;
    }

    /**
     * Convert it to Java.
     *
     * @return Java code
     */
    public String java() {
        return new UncheckedText(
            new FormattedText(
                "%s %s(%s)",
                this.type,
                this.name,
                new UncheckedText(
                    new JoinedText(
                        ", ",
                        new Mapped<>(
                            Parameter::java,
                            this.parameters
                        )
                    )
                ).asString()
            )
        ).asString();
    }

    /**
     * As XML.
     * @return Directives
     */
    public Iterable<Directive> xml() {
        return new XmlMethodDef(this.name, this.type, this.parameters);
    }
}
