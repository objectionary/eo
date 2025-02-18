/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2025 Objectionary.com
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

import com.github.lombrozo.xnav.Xnav;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import java.util.function.Consumer;
import org.w3c.dom.Node;

/**
 * This {@link Shift} finds all XPath matches and replaces them
 * with what a function suggests using {@link com.github.lombrozo.xnav.Xnav}.
 *
 * @since 0.53.0
 */
public final class StXnav implements Shift {
    /**
     * XPath to search for.
     */
    private final String xpath;

    /**
     * The mapping function.
     */
    private final Consumer<Xnav> fun;

    /**
     * Ctor.
     * @param path The XPath
     * @param func The function
     */
    public StXnav(final String path, final Consumer<Xnav> func) {
        this.xpath = path;
        this.fun = func;
    }

    @Override
    public String uid() {
        return this.getClass().getSimpleName();
    }

    @Override
    public XML apply(final int position, final XML xml) {
        final Node dom = xml.inner();
        new Xnav(dom).path(this.xpath).forEach(this.fun);
        return new XMLDocument(dom);
    }
}
