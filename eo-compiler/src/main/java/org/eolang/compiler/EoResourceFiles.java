/**
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;

/**
 * Represents all the example EO resource files.
 *
 * @author John Page (johnpagedev@gmail.com)
 * @version $Id$
 * @since 0.1
 */
public final class EoResourceFiles {

    /**
     * The path for the resource files.
     */
    private final String path;

    /**
     * Constructs an EoResourceFiles object.
     *
     * @param path The path for the resource files.
     */
    public EoResourceFiles(final String path) {
        this.path = path;
    }

    /**
     * Requests a formatted list of all the EO file names.
     *
     * @return A formatted string of all the file names.
     * @throws IOException If there is a problem reading the files.
     */
    public String formattedNames() throws IOException {
        final List<String> filenames = new ArrayList<>(0);
        try (
            BufferedReader br =
                new BufferedReader(
                    new InputStreamReader(new ResourceStream(this.path))
                )
        ) {
            String resource;
            resource = br.readLine();
            while (resource != null) {
                if ("eo".equals(FilenameUtils.getExtension(resource))) {
                    filenames.add(resource);
                }
                resource = br.readLine();
            }
        }
        return String.join("\n ", filenames);
    }

    /**
     * Requests the contents of an EO file.
     *
     * @param filename The name of the file.
     * @return A string of the contents of the file.
     * @throws IOException If this is some problem reading the file.
     */
    public String eolang(final String filename) throws IOException {
        return String.format(
            "\nEOLANG:\n%s",
            IOUtils.toString(
                new ResourceStream(this.path + filename),
                Charset.defaultCharset()
            ));
    }

    /**
     * Requests the contents of a EO file compiled into Java.
     *
     * @param filename The name of the file.
     * @return A string of the compiled contents.
     * @throws IOException If this is some problem reading the file.
     */
    public String java(final String filename) throws IOException {
        final StringBuilder java = new StringBuilder(0);
        java.append("\nJAVA:\n");
        final Program program = new Program(
            IOUtils.toString(
                new ResourceStream(this.path + filename),
                Charset.defaultCharset()
            )
        );
        program.save(new StringOutput(java));
        java.append('\n');
        return java.toString();
    }
}
