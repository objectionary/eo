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
import com.jcabi.xml.XMLDocument;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Unchecked;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Header of abstract object defined by the xmir file.
 * <p>abstract object must not be an atom.</p>
 * @since 0.21
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class EoHeader {

    /**
     * Header tag name.
     */
    public static final String HEADER_TAG = "header";

    /**
     * Headers tag name.
     */
    public static final String HEADERS_TAG = "headers";

    /**
     * Free attribute tag name.
     */
    public static final String FREE_ATTR_TAG = "free-attr";

    /**
     * Header in xml format.
     */
    private final Scalar<XML> xml;

    /**
     * Ctor.
     * @param xmir The XMIR
     */
    EoHeader(final XML xmir) {
        this.xml = new Sticky<>(
            () -> {
                try {
                    final Directives dirs = new Directives();
                    final String alias = new EoAlias(xmir).asString();
                    final XML node = xmir.nodes("/program/objects/o[not(@atom)]").get(0);
                    dirs.add(EoHeader.HEADER_TAG)
                        .attr("name", node.xpath("./@name").get(0))
                        .attr("alias", alias);
                    for (final XML attr : node.nodes("./o[not(@base)]")) {
                        dirs.add(EoHeader.FREE_ATTR_TAG)
                            .attr(
                                "name",
                                attr.xpath("./@name").get(0)
                            );
                        if (!attr.xpath("./@vararg").isEmpty()) {
                            dirs.attr("vararg", "");
                        }
                        dirs.up();
                    }
                    dirs.up();
                    return new XMLDocument(
                        new Xembler(dirs).xmlQuietly()
                    );
                } catch (final IndexOutOfBoundsException iob) {
                    throw new IllegalArgumentException(iob);
                }
            }
        );
    }

    /**
     * Header is valid.
     * @return Is valid or not
     */
    public boolean isValid() {
        boolean result = true;
        try {
            this.toXml();
        } catch (final IllegalArgumentException iae) {
            result = false;
        }
        return result;
    }

    /**
     * Transform to xml.
     * @return Header in xml format
     */
    public XML toXml() {
        return new Unchecked<>(this.xml).value();
    }
}
