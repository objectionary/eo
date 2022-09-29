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
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;

/**
 * Writes formulas from target/eo/03-optimize in .xmir form to .tex form.
 *
 * @since 0.1
 * @todo #1206:30min Add functions to LatexMojo to parse .xmir files to .tex
 * For new we can just create all needed .tex files
 * But we still need functionality to add phi-calculus formulas to these .tex files
 */
@Mojo(
    name = "latex",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true
)
public final class LatexMojo extends SafeMojo {

    /**
     * Target directory for all .tex files.
     */
    public static final Path TEX = new File("latex").toPath();

    /**
     * Main directory with all optimized .xmir files.
     */
    public static final Path SOURCE = new File("03-optimize/org/eolang").toPath();

    @Override
    void exec() throws IOException {
        final Path target = this.targetDir.toPath().resolve(LatexMojo.TEX);
        final Path source = this.targetDir.toPath().resolve(LatexMojo.SOURCE);
        final String pattern = LatexMojo.SOURCE.toString();
        if (!Files.exists(target)) {
            new File(target.toString()).mkdirs();
            new File(target.resolve("universe.tex").toString()).createNewFile();
            Logger.info(
                this,
                "Created %s directory",
                target
            );
        }
        if (Files.exists(source)) {
            final List<Path> files = new Walk(source);
            for (final Path file : files) {
                final int start = file.toString().indexOf(pattern) + pattern.length() + 1;
                final String name = file.toString().substring(start);
                final String tex = name.replace(".xmir", ".tex");
                final Path put = target.resolve(tex);
                final String fname = new File(file.toString()).getName();
                if (!fname.equals(name)) {
                    final String subdir = name.replace(fname, "");
                    final Path path = target.resolve(subdir);
                    new File(path.toString()).mkdirs();
                }
                new File(put.toString()).createNewFile();
            }
            Logger.info(
                this,
                "%d .xmir files are translated to .tex files in %s directory",
                files.size(),
                target
            );
        }
    }
}
