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

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Writes formulas from target/eo/03-optimize in .tex form.
 *
 * @since 0.1
 * @todo #1206:30min Add functions to LaxtexMogo to parse .xmir files to .tex
 * For new we can just create all needed .tex files
 * But we still need functionality to add phi-calculus furmulas to these .tex files
 */
public final class LatexMojo extends SafeMojo {

    /**
     * Target directory for all .tex files.
     */
    public static final String TEX = "eo-runtime/target/eo/latex/";

    /**
     * Main directory with all optimized .xmir files.
     */
    public static final String DIR = "eo-runtime/target/eo/03-optimize/org/eolang";

    /**
     * Recursive function to get all files from directory to move them to .tex form.
     * @param files The list of files inside the current directory.
     * @param subdir The name of prefix to add to .xmir files.
     * @throws IOException Returns IOException.
     */
    static void rec(final File[] files, final String subdir) throws IOException {
        for (final File child: files) {
            if (child.isDirectory()) {
                new File(LatexMojo.TEX.concat(subdir).concat(child.getName())).mkdirs();
                rec(child.listFiles(), subdir.concat(child.getName()).concat("/"));
                continue;
            }
            final String name = child.getName();
            final String unext = name.substring(0, name.lastIndexOf('.'));
            final String put = LatexMojo.TEX.concat(subdir).concat(unext).concat(".tex");
            new File(put).createNewFile();
        }
    }

    @Override
    void exec() throws IOException {
        if (!Files.exists(Paths.get(LatexMojo.TEX))) {
            new File(LatexMojo.TEX).mkdirs();
            new File(LatexMojo.TEX.concat("universe.tex")).createNewFile();
        }
        if (Files.exists(Paths.get(LatexMojo.DIR))) {
            final File[] files = new File(LatexMojo.DIR).listFiles();
            rec(files, "");
        }
    }
}
