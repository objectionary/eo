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

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XSLChain;
import com.jcabi.xml.XSLDocument;
import java.nio.file.Path;
import java.nio.file.Paths;
import org.cactoos.io.InputOf;
import org.cactoos.io.TeeInput;
import org.cactoos.list.ListOf;
import org.cactoos.list.Mapped;
import org.cactoos.scalar.LengthOf;
import org.cactoos.scalar.Unchecked;

/**
 * ToJava.
 *
 * @since 0.1
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
public final class ToJava {

    /**
     * The XML with EO parsed already.
     */
    private final XML xml;

    /**
     * Directory to save Java files to.
     */
    private final Path dir;

    /**
     * Ctor.
     *
     * @param input Input text
     * @param file The file to write the XML to
     */
    public ToJava(final XML input, final Path file) {
        this.xml = input;
        this.dir = file;
    }

    /**
     * Compile it to XML and save.
     */
    public void compile() {
        final XML out = new XSLChain(
            new Mapped<>(
                node -> new XSLDocument(
                    ToJava.class.getResourceAsStream(node)
                ),
                new ListOf<>(
                    "errors/broken-aliases.xsl",
                    "errors/duplicate-aliases.xsl",
                    "errors/one-body.xsl",
                    "errors/reserved-atoms.xsl",
                    "errors/same-line-names.xsl",
                    "errors/self-naming.xsl",
                    "01-add-refs.xsl",
                    "02-resolve-aliases.xsl",
                    "errors/unknown-names.xsl",
                    "03-abstracts-float-up.xsl",
                    "04-rename-bases.xsl",
                    "05-wrap-method-calls.xsl",
                    "06-to-java.xsl"
                )
            )
        ).transform(this.xml);
        for (final XML error : out.nodes("/program/errors/error")) {
            Logger.error(
                this,
                "[%s] %s",
                error.xpath("@line").get(0),
                error.xpath("text()").get(0)
            );
        }
        for (final XML file : out.nodes("/program/objects/o[java]")) {
            ToJava.save(
                this.dir.resolve(
                    Paths.get(
                        String.format(
                            "%s.java", file.xpath("@name").get(0)
                        )
                    )
                ),
                file.xpath("java/text()").get(0)
            );
        }
    }

    /**
     * Save one Java file.
     * @param path The path
     * @param content The content
     */
    private static void save(final Path path, final String content) {
        new Unchecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(content),
                    path
                )
            )
        ).value();
        Logger.info(
            ToJava.class,
            "Saved %d chars to %s",
            content.length(),
            path.toAbsolutePath()
        );
    }

}
