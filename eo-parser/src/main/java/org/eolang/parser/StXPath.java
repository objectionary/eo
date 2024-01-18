/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2024 Objectionary.com
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
package org.eolang.parser;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import java.util.List;
import java.util.function.Function;
import org.xembly.Directive;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * This {@link Shift} finds all XPath matches and replaces them
 * with what a function suggests.
 *
 * @since 0.29.0
 */
public final class StXPath implements Shift {

    /**
     * XPath to search for.
     */
    private final String xpath;

    /**
     * The mapping function.
     */
    private final Function<XML, Iterable<Directive>> fun;

    /**
     * Ctor.
     * @param path The XPath
     * @param func The function
     */
    public StXPath(final String path, final Function<XML, Iterable<Directive>> func) {
        this.xpath = path;
        this.fun = func;
    }

    @Override
    public String uid() {
        return this.getClass().getSimpleName();
    }

    @Override
    public XML apply(final int position, final XML xml) {
        final List<XML> nodes = xml.nodes(this.xpath);
        if (nodes.size() > 1) {
            throw new IllegalArgumentException(
                String.format(
                    "XPath '%s' returned too many elements (%d)",
                    this.xpath, nodes.size()
                )
            );
        }
        final Directives dirs = new Directives();
        if (!nodes.isEmpty()) {
            dirs.xpath(this.xpath);
            dirs.append(this.fun.apply(nodes.get(0)));
        }
        return new XMLDocument(
            new Xembler(dirs).applyQuietly(xml.node())
        );
    }

}

