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
import java.nio.file.Path;
import java.util.List;
import org.cactoos.Scalar;

/**
 * Footprint is a term used to refer to a trace of an EO program on the file system.
 *
 * <p>It's a fitting analogy with the word's natural meaning, and what it essentially does is trace
 * files that have a specific extension. Footprint enables you to do the following:
 * - Save files that have a specific extension
 * - Load files that have a specific extension
 * - Get a complete list of files that have a specific extension</p>
 *
 * <p>You can find a full list of available methods below.</p>
 *
 * <p>Since Footprint can leave traces on the file system, each Footprint instance will have its
 * own unique way of saving and loading files, and it will have its own place to do so.
 * For instance, {@link FtDefault} will save files directly to a specific location on the file
 * system, while {@link FtCached} will first check the cache directory before delegating
 * the behavior.
 * In other words, Footprint provides useful functionality for working with files that
 * have different extensions.</p>
 *
 * @since 1.0
 */
public interface Footprint {

    /**
     * Get program content of a specific type.
     *
     * <p>That is, if the file contains "hello", it will return the string "hello".</p>
     *
     * @param program Program name
     * @param ext File extension which defines the type
     * @return Content of a file
     * @throws IOException In case of IO issue.
     */
    String load(String program, String ext) throws IOException;

    /**
     * Save content.
     *
     * <p>Leaves a "footprint" in the directory.
     * So it can create a new file in the file system.
     * Where it will create this file depends on the implementation.</p>
     *
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
