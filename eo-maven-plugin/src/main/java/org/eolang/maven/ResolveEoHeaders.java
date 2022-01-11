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
import java.util.List;
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
 * Resolves EO headers of an eo file.
 *
 * @since 0.21
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings("PMD.AvoidDuplicateLiterals")
final class ResolveEoHeaders {

    /**
     * The register.
     */
    private final XML register;

    /**
     * The EO file.
     */
    private final Input input;

    /**
     * EO file updated.
     */
    private final Output output;

    /**
     * Ctor.
     * @param register Register
     * @param input Input
     * @param output Output
     */
    ResolveEoHeaders(
        final XML register, final Input input, final Output output
    ) {
        this.register = register;
        this.input = input;
        this.output = output;
    }

    /**
     * Executes.
     */
    public void exec() {
        final String content = new TextOf(this.input).toString();
        XML xmir = new XMLDocument(
            new Xembler(
                new Directives().xpath("/program").add(EoHeader.HEADERS_TAG).up()
            ).applyQuietly(new XMLDocument(content).node())
        );
        for (
            final String alias : xmir.xpath(
                "/program/metas/meta[head = 'alias']/part[last()]/text()"
            )
        ) {
            final List<XML> headers = this.register.nodes(
                String.format(
                    "/%s/%s[@alias='%s']",
                    EoHeader.HEADERS_TAG,
                    EoHeader.HEADER_TAG,
                    alias
                )
            );
            if (headers.isEmpty()) {
                continue;
            }
            final XML header = headers.get(0);
            xmir = new XMLDocument(
                new Xembler(
                    new Directives().xpath(
                        String.format("/program/%s", EoHeader.HEADERS_TAG)
                    )
                    .add(EoHeader.HEADER_TAG)
                    .append(header.node())
                ).applyQuietly(xmir.node())
            );
        }
        final String alias = new EoAlias(xmir).asString();
        for (final XML node : xmir.nodes("/program/objects/o")) {
            final List<XML> headers = this.register.nodes(
                String.format(
                    "/%s/%s[@name='%s' and @alias='%s']",
                    EoHeader.HEADERS_TAG,
                    EoHeader.HEADER_TAG,
                    node.xpath("./@name").get(0),
                    alias
                )
            );
            if (headers.isEmpty()) {
                continue;
            }
            final XML header = headers.get(0);
            xmir = new XMLDocument(
                new Xembler(
                    new Directives().xpath(
                        String.format("/program/%s", EoHeader.HEADERS_TAG)
                    )
                    .add(EoHeader.HEADER_TAG)
                    .append(header.node())
                ).applyQuietly(xmir.node())
            );
        }
        new Unchecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(
                        xmir.toString()
                    ),
                    this.output
                )
            )
        ).value();
    }
}
