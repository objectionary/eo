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
package org.eolang.compiler;

import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XSL;
import com.jcabi.xml.XSLDocument;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedList;
import org.cactoos.list.ListOf;
import org.cactoos.list.Mapped;

/**
 * Pack of all XSL sheets.
 *
 * @since 0.1
 */
public final class Pack implements Iterable<XSL> {

    /**
     * All of them.
     */
    private final Collection<String> sheets;

    /**
     * Ctor, with the default sequence of sheets.
     */
    @SuppressWarnings("PMD.AvoidDuplicateLiterals")
    public Pack() {
        this(
            new ListOf<>(
                "errors/not-empty-atoms.xsl",
                "errors/middle-varargs.xsl",
                "errors/duplicate-names.xsl",
                "errors/broken-aliases.xsl",
                "errors/duplicate-aliases.xsl",
                "errors/global-nonames.xsl",
                "errors/same-line-names.xsl",
                "errors/self-naming.xsl",
                "add-refs.xsl",
                "wrap-method-calls.xsl",
                "vars-float-up.xsl",
                "add-refs.xsl",
                "resolve-aliases.xsl",
                "add-default-package.xsl",
                "errors/broken-refs.xsl",
                "errors/unknown-names.xsl",
                "errors/noname-attributes.xsl"
            )
        );
    }

    /**
     * Ctor.
     *
     * @param names Names of them in classpath
     */
    public Pack(final Collection<String> names) {
        this.sheets = Collections.unmodifiableCollection(names);
    }

    @Override
    public Iterator<XSL> iterator() {
        return new Mapped<>(
            doc -> new XSLDocument(
                Program.class.getResourceAsStream(doc)
            ).with(new ClasspathSources()),
            this.sheets
        ).iterator();
    }

    /**
     * Make a copy, with an additional XSL.
     * @param xsl Name in classpath
     * @return New pack
     */
    public Pack with(final String xsl) {
        final Collection<String> after = new LinkedList<>(this.sheets);
        after.add(xsl);
        return new Pack(after);
    }
}
