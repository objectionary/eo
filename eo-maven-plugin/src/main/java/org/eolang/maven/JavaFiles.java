/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import org.cactoos.text.Joined;

/**
 * Java files from XMIR.
 * @since 1.0
 */
public final class JavaFiles {

    /**
     * Path to XMIR file.
     */
    private final Path source;

    /**
     * Base destination path.
     */
    private final Path dest;

    /**
     * Ctor.
     *
     * @param src XML with java code
     * @param target Base destination path
     */
    public JavaFiles(final Path src, final Path target) {
        this.source = src;
        this.dest = target;
    }

    /**
     * Save java files.
     * @return Count of saved files
     * @throws IOException In case issues with I/O
     */
    public int save() throws IOException {
        int total = 0;
        final XML xml = new XMLDocument(this.source);
        final Collection<XML> nodes = xml.nodes("//class[java and not(@atom)]");
        if (nodes.isEmpty()) {
            Logger.debug(
                this, "No .java files generated from %s",
                Save.rel(this.source)
            );
        } else {
            for (final XML java : nodes) {
                JavaFiles.saveJava(java, this.dest);
                ++total;
            }
            Logger.info(
                this, "Generated %d .java file(s) from %s to %s",
                nodes.size(), Save.rel(this.source), Save.rel(this.dest)
            );
        }
        return total;
    }

    /**
     * Save this Java file.
     * @param java The XML with Java
     * @param generated Path to all files
     * @throws IOException If fails
     */
    private static void saveJava(final XML java, final Path generated) throws IOException {
        final String type = java.xpath("@java-name").get(0);
        final Path dest = new Place(type).make(
            generated, "java"
        );
        new Save(
            new Joined(
                "",
                java.xpath("java/text()")
            ),
            dest
        ).save();
    }
}
