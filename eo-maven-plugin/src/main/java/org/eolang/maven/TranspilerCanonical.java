/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2021 Yegor Bugayenko
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
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.List;
import org.cactoos.io.OutputTo;
import org.cactoos.list.ListOf;
import org.eolang.parser.Xsline;

/**
 * Native compiler.
 *
 * @since 0.1
 */
final class TranspilerCanonical implements Transpiler {

    /**
     * Temp dir.
     */
    private final Path temp;

    /**
     * Dir with pre-files.
     */
    private final Path pre;

    /**
     * Ctor.
     * @param tmp The temp
     * @param ppre The pre
     */
    TranspilerCanonical(final Path tmp, final Path ppre) {
        this.temp = tmp;
        this.pre = ppre;
    }

    @Override
    public int transpile(final Path file, final Path generated) throws IOException {
        final XML input = new XMLDocument(file);
        final String name = input.xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Path target = place.make(this.temp, Transpiler.EXT);
        int total = 0;
        if (
            target.toFile().exists()
                && target.toFile().lastModified() >= file.toFile().lastModified()
        ) {
            Logger.info(
                this, "XMIR %s (%s) already transpiled to %s",
                Save.rel(file), name, Save.rel(target)
            );
        } else {
            new Xsline(
                input,
                new OutputTo(target),
                new TargetSpy(place.make(this.pre, "")),
                new ListOf<>(
                    "org/eolang/maven/pre/classes.xsl",
                    "org/eolang/maven/pre/junit.xsl",
                    "org/eolang/maven/pre/attrs.xsl",
                    "org/eolang/maven/pre/varargs.xsl",
                    "org/eolang/maven/pre/data.xsl",
                    "org/eolang/maven/pre/to-java.xsl"
                )
            ).pass();
            final XML after = this.noErrors(new XMLDocument(target), name);
            final Collection<XML> nodes = after.nodes("//class[java and not(@atom)]");
            if (nodes.isEmpty()) {
                Logger.info(
                    this, "Transpiled %s but no .java files created",
                    Save.rel(file)
                );
            } else {
                for (final XML java : nodes) {
                    new Save(
                        java.xpath("java/text()").get(0),
                        new Place(java.xpath("@java-name").get(0)).make(
                            generated, "java"
                        )
                    ).save();
                    ++total;
                }
                Logger.info(
                    this, "Transpiled %s to %s, created %d .java file(s)",
                    Save.rel(file), Save.rel(generated), nodes.size()
                );
            }
        }
        return total;
    }

    /**
     * Check for errors.
     *
     * @param xml The XML output
     * @param name Name of the program
     * @return The same XML if no errors
     */
    private XML noErrors(final XML xml, final String name) {
        final List<XML> errors = xml.nodes("/program/errors/error");
        for (final XML error : errors) {
            Logger.error(
                this,
                "[%s:%s] %s (%s:%s)",
                name,
                error.xpath("@line").get(0),
                error.xpath("text()").get(0),
                error.xpath("@check").get(0),
                error.xpath("@step").get(0)
            );
        }
        if (!errors.isEmpty()) {
            throw new IllegalStateException(
                String.format(
                    "There are %d errors in %s, see log above",
                    errors.size(), name
                )
            );
        }
        return xml;
    }

}
