/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2023 Objectionary.com
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
import java.nio.file.Path;
import java.util.List;
import org.cactoos.Scalar;

/**
 * Program footprint of EO compilation process,
 * convenient eo program management mechanism.
 * @since 1.0
 */
public interface Footprint {

    /**
     * Get program content of a specific type.
     * @param program Program name
     * @param ext File extension which defines the type
     * @return Content of a file
     * @throws IOException In case of IO issue.
     */
    String load(String program, String ext) throws IOException;

    /**
     * Save content. Leaves a "footprint" in the directory.
     * @param program Program name
     * @param ext File extension
     * @param content File content
     * @throws IOException In case of IO issues
     */
    void save(String program, String ext, Scalar<String> content) throws IOException;

    /**
     * Get list of saved regular files with ext.
     * @param ext File extension
     * @return List of files
     * @throws IOException In case of IO issues
     */
    List<Path> list(String ext) throws IOException;
}
