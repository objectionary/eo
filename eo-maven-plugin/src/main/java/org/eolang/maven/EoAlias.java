/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Yegor Bugayenko
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
package org.eolang.maven;

import com.jcabi.xml.XML;
import java.util.List;
import org.cactoos.Text;

/**
 * EO alias.
 * @since 0.21
 */
final class EoAlias implements Text {

    /**
     * The XMIR.
     */
    private final XML xmir;

    /**
     * Ctor.
     * @param xmir The XMIR
     */
    EoAlias(final XML xmir) {
        this.xmir = xmir;
    }

    @Override
    public String asString() {
        final List<String> pkgs =
            this.xmir.xpath("/program/metas/meta[head='package']/tail/text()");
        final String name = this.xmir.xpath("/program/objects/o/@name").get(0);
        final String alias;
        if (pkgs.isEmpty()) {
            alias = name;
        } else {
            alias = String.format("%s.%s", pkgs.get(0), name);
        }
        return alias;
    }
}
