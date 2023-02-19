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
package org.eolang.maven;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collection;
import java.util.regex.Pattern;

import com.yegor256.tojos.Tojo;
import com.yegor256.xsline.Xsline;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.text.TextOf;
import org.eolang.maven.util.Home;

/**
 * Compile binaries.
 *
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 * @since 0.1
 * @todo #1161:30m Extract Rust code & parameters
 *  from org.eolang.rust objects here.
 *  Call rustc with provided dependencies and
 *  put binary *.so files to target directory.
 */
@Mojo(
    name = "binarize",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
@SuppressWarnings("PMD.LongVariable")
public final class BinarizeMojo extends SafeMojo {

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    @Parameter(
        required = true,
        defaultValue = "${project.build.directory}/eo-binaries"
    )
    @SuppressWarnings("PMD.UnusedPrivateField")
    private File generatedDir;

    /**
     * The directory where to binarize to.
     */
    public static final String DIR = "binarize/";

    private static Pattern pattern;

    @Override
    public void exec() throws IOException {
        //throw new UnsupportedEncodingException("NYI");
        System.out.println("\nHello from BinarizeMojo");
        final Collection<Tojo> sources = this.scopedTojos().select(
                row -> row.exists(AssembleMojo.ATTR_XMIR)
        );
        for (Tojo tojo: sources) {
            final Path src = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR));
            System.out.println("src = " + src);

            String content = new TextOf(src).toString();

            if (src.toString().contains("rust")) {
                final Path dir = this.targetDir.toPath().resolve(BinarizeMojo.DIR + "/lib" + src.hashCode() + ".rs");
                System.out.println("dir = " + dir);
                new Home(dir).save("I am rust file from src = " + src, dir);
                System.out.println("Content: ");
                System.out.println(content);
            }

        }

        System.out.println("Bye from BinarizeMojo\n");
    }

}
