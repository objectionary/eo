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

import java.util.Iterator;
import org.eolang.compiler.syntax.Method;
import org.xembly.Directive;
import org.xembly.Directives;

/**
 * Type def as XML.
 * @author Kirill (g4s8.public@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class XmlTypeDef implements Iterable<Directive> {
    /**
     * Type name.
     */
    private final String name;
    /**
     * Type supertypes.
     */
    private final Iterable<String> spr;
    /**
     * Type methods.
     */
    private final Iterable<Method> methods;
    /**
     * Ctor.
     * @param name Name
     * @param spr Supertypes
     * @param methods Methods
     */
    public XmlTypeDef(final String name,
        final Iterable<String> spr, final Iterable<Method> methods) {
        this.name = name;
        this.spr = spr;
        this.methods = methods;
    }

    @Override
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    public Iterator<Directive> iterator() {
        final Directives dir = new Directives().add("type")
            .attr("name", this.name);
        dir.add("super");
        for (final String type : this.spr) {
            dir.add("type").attr("name", type).up();
        }
        dir.up();
        dir.add("methods");
        for (final Method method : this.methods) {
            dir.append(method.xml());
        }
        dir.up();
        return dir.up().iterator();
    }
}
