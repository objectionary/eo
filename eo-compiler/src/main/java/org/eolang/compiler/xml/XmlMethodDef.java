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
package org.eolang.compiler.xml;

import java.util.Collection;
import java.util.Iterator;
import org.eolang.compiler.syntax.Parameter;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Method def as XML.
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class XmlMethodDef implements Iterable<Directive> {
    /**
     * Parameters.
     */
    private final Collection<Parameter> params;
    /**
     * Method type.
     */
    private final String type;
    /**
     * Method name.
     */
    private final String name;
    /**
     * Ctor.
     * @param name Name
     * @param type Type
     * @param parameters Parameters
     */
    public XmlMethodDef(final String name,
        final String type, final Collection<Parameter> parameters) {
        this.name = name;
        this.type = type;
        this.params = parameters;
    }

    @Override
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    public Iterator<Directive> iterator() {
        final Directives dir = new Directives()
            .add("method")
            .attr("name", this.name)
            .add("type")
            .attr("name", this.type)
            .up();
        dir.add("params");
        for (final Parameter param : this.params) {
            dir.append(param.xml());
        }
        dir.up();
        return dir.up().iterator();
    }
}
