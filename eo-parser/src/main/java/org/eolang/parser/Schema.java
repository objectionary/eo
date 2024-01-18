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

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XSDDocument;
import java.util.Collection;
import javax.xml.transform.dom.DOMSource;
import org.cactoos.io.ResourceOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;
import org.xml.sax.SAXParseException;

/**
 * The schema for XMIR.
 *
 * @since 0.6
 */
public final class Schema {

    /**
     * The XML.
     */
    private final XML xmir;

    /**
     * Ctor.
     * @param xml The XMIR
     */
    public Schema(final XML xml) {
        this.xmir = xml;
    }

    /**
     * Check and crash if mistakes.
     */
    public void check() {
        final Collection<SAXParseException> violations = new XSDDocument(
            new UncheckedText(
                new TextOf(new ResourceOf("XMIR.xsd"))
            ).asString()
        ).validate(new DOMSource(this.xmir.node()));
        if (!violations.isEmpty()) {
            Logger.error(this, "XML with XSD failures:%n%s", this.xmir);
            for (final SAXParseException violation : violations) {
                Logger.error(
                    this, "XSD failure at #%d:%d %s",
                    violation.getLineNumber(),
                    violation.getColumnNumber(),
                    violation.getLocalizedMessage()
                );
            }
            throw new IllegalStateException(
                String.format(
                    "There are %d XSD violation(s), see the log",
                    violations.size()
                )
            );
        }
    }

}
