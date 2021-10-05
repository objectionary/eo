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
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import org.cactoos.io.InputOf;
import org.cactoos.io.OutputTo;
import org.eolang.parser.Syntax;

/**
 * One .EO to .EO.XML parsing.
 *
 * @since 0.1
 */
final class Parsing {

    /**
     * The source .eo file.
     */
    private final Path source;

    /**
     * The directory with protocols.
     */
    private final Path protocols;

    /**
     * Ctor.
     * @param src The source
     * @param protos The dir with protocols
     */
    Parsing(final Path src, final Path protos) {
        this.source = src;
        this.protocols = protos;
    }

    /**
     * Parse EO file to XML.
     *
     * @param target The "/target" directory
     * @param name Object name, like "org.eolang.io.stdout"
     */
    void into(final Path target, final String name) {
        final Path path = new Place(name).make(
            target.resolve(ParseMojo.DIR), "eo.xml"
        );
        if (Files.exists(path)) {
            Logger.info(this, "%s already parsed to %s", this.source, path);
        } else {
            try {
                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                new Syntax(
                    name,
                    new InputOf(this.source),
                    new OutputTo(baos)
                ).parse();
                new Save(baos.toByteArray(), path).save();
                new Save(
                    String.join(
                        "\n",
                        "action: parse",
                        String.format("file: %s", this.source),
                        String.format("time: %s", Instant.now().toString())
                    ),
                    new Place(name).make(this.protocols, "log")
                ).save();
            } catch (final IOException ex) {
                throw new IllegalStateException(
                    String.format(
                        "Can't parse %s into %s",
                        this.source, target
                    ),
                    ex
                );
            }
            Logger.info(this, "%s parsed to %s", this.source, path);
        }
    }

}
