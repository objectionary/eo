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
package org.eolang.maven.footprint;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import org.cactoos.Scalar;
import org.cactoos.scalar.IoChecked;
import org.cactoos.text.IoCheckedText;
import org.cactoos.text.TextOf;
import org.eolang.maven.Place;
import org.eolang.maven.util.HmBase;

/**
 * Default implementation of a Footprint.
 * Program footprint of EO compilation process.
 * <p>The footprint consists of file in {@link #main} folder</p>
 * @since 1.0
 * @checkstyle NoJavadocForOverriddenMethodsCheck (100 lines)
 */
public final class FtDefault implements Footprint {

    /**
     * Path to main location.
     */
    private final Path main;

    /**
     * Ctor.
     * @param path Main location.
     */
    public FtDefault(final Path path) {
        this.main = path;
    }

    @Override
    public String load(final String program, final String ext) throws IOException {
        return new IoCheckedText(
            new TextOf(
                new Place(program).make(this.main, ext)
            )
        ).asString();
    }

    @Override
    public void save(final String program, final String ext, final Scalar<String> content)
        throws IOException {
        new HmBase(this.main).save(
            new IoChecked<>(content).value(),
            this.main.relativize(new Place(program).make(this.main, ext))
        );
    }

    /**
     * Get list of saved regular files with ext.
     *
     * @param ext File extension
     * @return List of files
     * @throws IOException In case of IO issues
     */
    @Override
    public List<Path> list(final String ext) throws IOException {
        final List<Path> res;
        if (Files.exists(this.main)) {
            try (Stream<Path> walk = Files.walk(this.main)) {
                res = walk.filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(ext))
                    .collect(Collectors.toList());
            }
        } else {
            res = Collections.emptyList();
        }
        return res;
    }
}
