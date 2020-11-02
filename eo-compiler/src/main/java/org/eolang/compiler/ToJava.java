/*
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
            new ListOf<>(
                new XSLDocument(
                    ToJava.class.getResourceAsStream(
                        "resolve-names.xsl"
                    )
                ),
                new XSLDocument(
                    ToJava.class.getResourceAsStream(
                        "to-java.xsl"
                    )
                )
            )
        ).transform(this.xml);
        Logger.info(this, out.toString());
        for (final XML file : out.nodes("/package/file")) {
            ToJava.save(
                this.dir.resolve(Paths.get(file.xpath("@name").get(0))),
                file.xpath("text()").get(0)
            );
        }
    }

    private static void save(final Path path, final String content) {
        Logger.info(ToJava.class, "Saving to %s", path.toAbsolutePath());
        new Unchecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(content),
                    path
                )
            )
        ).value();
    }

}
