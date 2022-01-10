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
import org.cactoos.Input;
import org.cactoos.Output;
import org.cactoos.io.InputOf;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;
import org.cactoos.text.TextOf;
import org.xembly.Directives;
import org.xembly.Xembler;

/**
 * Registry of EO headers.
 * @since 0.21
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
final class EoHeaderRegistry {

    /**
     * EO header to register.
     */
    private final EoHeader header;

    /**
     * Initial content of registry.
     */
    private final Input input;

    /**
     * Final content of registry.
     */
    private final Output output;

    /**
     * Ctor.
     * @param header EO header to register
     * @param input Initial content of registry
     * @param output Final content of registry
     */
    EoHeaderRegistry(
        final EoHeader header, final Input input, final Output output
    ) {
        this.header = header;
        this.input = input;
        this.output = output;
    }

    /**
     * Register headers contained in XMIR.
     */
    public void register() {
        final XML def = this.header.toXml();
        String content = new TextOf(this.input).toString();
        if (content.isEmpty()) {
            content = String.join(
                "\n",
                "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                "<headers>",
                "</headers>"
            );
        }
        XML registry = new XMLDocument(content);
        if (
            registry
                .xpath(
                    String.format(
                        "/%s/%s[@name='%s' and @alias='%s']/text()",
                        EoHeader.HEADERS_TAG,
                        EoHeader.HEADER_TAG,
                        def.xpath(
                            String.format("/%s/@name", EoHeader.HEADER_TAG)
                        ).get(0),
                        def.xpath(
                            String.format("/%s/@alias", EoHeader.HEADER_TAG)
                        ).get(0)
                    )
                ).isEmpty()
        ) {
            registry = new XMLDocument(
                new Xembler(
                    new Directives().xpath(
                        String.format("/%s", EoHeader.HEADERS_TAG)
                    ).append(def.node())
                ).applyQuietly(registry.node())
            );
        }
        new Unchecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(
                        registry.toString()
                    ),
                    this.output
                )
            )
        ).value();
    }
}
