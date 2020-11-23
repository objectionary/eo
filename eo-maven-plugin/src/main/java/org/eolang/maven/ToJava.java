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
package org.eolang.maven;

import com.jcabi.log.Logger;
import com.jcabi.xml.ClasspathSources;
import com.jcabi.xml.XML;
import com.jcabi.xml.XSLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.cactoos.io.TeeInput;
import org.cactoos.scalar.IoChecked;
import org.cactoos.scalar.LengthOf;

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
     * Directory to save temp files to.
     */
    private final Path temp;

    /**
     * Ctor.
     *
     * @param input Input text
     * @param file The file to write the XML to
     * @param tmp Temp dir
     */
    public ToJava(final XML input, final Path file, final Path tmp) {
        this.xml = input;
        this.dir = file;
        this.temp = tmp;
    }

    /**
     * Compile it to XML and save.
     * @throws IOException If fails
     */
    public void compile() throws IOException {
        final XML out = new XSLDocument(
            ToJava.class.getResourceAsStream("to-java.xsl")
        ).with(new ClasspathSources()).transform(this.xml);
        new IoChecked<>(
            new LengthOf(
                new TeeInput(
                    new InputOf(out.toString()),
                    new OutputTo(
                        this.temp.resolve(
                            String.format(
                                "%s.xml",
                                this.xml.xpath("/program/@name").get(0)
                            )
                        )
                    )
                )
            )
        ).value();
        final List<XML> errors = out.nodes("/program/errors/error");
        for (final XML error : errors) {
            Logger.error(
                this,
                "[%s] %s",
                error.xpath("@line").get(0),
                error.xpath("text()").get(0)
            );
        }
        if (!errors.isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "There are %d errors, see log above",
                    errors.size()
                )
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
     * @throws IOException If fails
     */
    private static void save(final Path path, final String content)
        throws IOException {
        new IoChecked<>(
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
