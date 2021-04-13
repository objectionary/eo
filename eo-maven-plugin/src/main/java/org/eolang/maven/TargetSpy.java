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
import com.jcabi.xml.XSL;
import org.eolang.parser.Spy;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;

/**
 * The spy to log all results.
 *
 * @since 0.1
 */
final class TargetSpy implements Spy {

    /**
     * The dir.
     */
    private final Path dir;

    /**
     * Ctor.
     * @param target The path
     */
    TargetSpy(final Path target) {
        this.dir = target;
    }

    @Override
    public void push(final int index, final XSL xsl, final XML xml)
        throws IOException {
        final List<String> names = new XMLDocument(
            xsl.toString()
        ).xpath("/*/@id");
        final String file;
        if (names.isEmpty()) {
            file = String.format("%d", index);
        } else {
            file = names.get(0).replaceAll("[^a-z0-9]", "-");
        }
        if (this.dir.toFile().mkdirs()) {
            Logger.debug(this, "Directory %s created", this.dir);
        }
        new Save(
            xml.toString(),
            this.dir.resolve(
                String.format("%02d-%s.xml", index, file)
            )
        ).save();
        Logger.debug(this, "Step #%d by %s:\n%s", index, file, xml);
    }
}
