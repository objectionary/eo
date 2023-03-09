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

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.tojos.Tojo;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrBulk;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Hex;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.eolang.maven.util.Home;
import org.eolang.maven.util.Rel;
import org.eolang.parser.ParsingTrain;

/**
 * Compile binaries.
 *
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 * @since 0.1
 *
 * @todo #1876:90m Expand add-rust.xsl. Now
 *  it creates a section 'program/rusts' with nodes
 *  'rust' with attribute 'code'. It is also necessary
 *  to add dependencies to every 'rust'
 *
 * @todo #1864:90m Extract rust code from rust section
 *  BinarizeMojo firstly put the code into rust section in xmir.
 *  Then it must be compiled to shared library. It can be
 *  implemented via cargo.
 */
@Mojo(
    name = "binarize",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)
@SuppressWarnings("PMD.LongVariable")
public final class BinarizeMojo extends SafeMojo implements CompilationStep {

    /**
     * The directory where to binarize to.
     */
    public static final String DIR = "binarize";

    /**
     * Parsing train with XSLs.
     */
    static final Train<Shift> TRAIN = new TrBulk<>(
        new TrClasspath<>(
            new ParsingTrain()
                .empty()
        ),
        Arrays.asList(
            "/org/eolang/maven/add_rust/add_rust.xsl"
        )
    ).back().back();

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

    @Override
    public void exec() throws IOException {
        final Collection<Tojo> sources = this.tojos.value().select(
            row -> row.exists(AssembleMojo.ATTR_XMIR2)
                && row.get(AssembleMojo.ATTR_SCOPE).equals(this.scope)
        );
        for (final Tojo tojo : sources) {
            final Path file = Paths.get(tojo.get(AssembleMojo.ATTR_XMIR2));
            final XML input = new XMLDocument(file);
            final String name = input.xpath("/program/@name").get(0);
            final Place place = new Place(name);
            final Path target = place.make(
                this.targetDir.toPath().resolve(BinarizeMojo.DIR),
                "rs"
            );
            final Path src = Paths.get(tojo.get(AssembleMojo.ATTR_EO));
            if (
                target.toFile().exists()
                    && target.toFile().lastModified() >= file.toFile().lastModified()
                    && target.toFile().lastModified() >= src.toFile().lastModified()
            ) {
                Logger.info(
                    this, "XMIR %s (%s) were already binarized to %s",
                    new Rel(file), name, new Rel(target)
                );
            } else {
                for (final String rust: this.addRust(input, target)) {
                    new Home(target).save(unhex(rust), target);
                }
            }
        }
    }

    /**
     * Creates a "rust" section in xml file and returns its content.
     * @param input The .xmir file
     * @param target The path to put the result file
     * @return The content of rust section
     * @throws IOException If any issues with I/O
     */
    private List<String> addRust(
        final XML input,
        final Path target
    ) throws IOException {
        final String name = input.xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Train<Shift> trn = new SpyTrain(
            BinarizeMojo.TRAIN,
            place.make(this.targetDir.toPath().resolve(BinarizeMojo.DIR), "")
        );
        final Path dir = this.targetDir.toPath().resolve(BinarizeMojo.DIR);
        final XML passed = new Xsline(trn).pass(input);
        new Home(dir).save(passed.toString(), dir.relativize(target));

        final List<XML> nodes = passed.nodes("/program/rusts/rust");
        System.out.println("Printing nodes' @code, nodes len = " + nodes.size());
        for (final XML node: nodes) {
            System.out.println(node.xpath("@code"));
        }

        return passed.xpath("/program/rusts/rust/@code");
    }

    /**
     * Makes a text from Hexed text.
     * @param txt Hexed chars separated by backspace.
     * @return Normal text.
     */
    private static String unhex(final String txt) {
        final StringBuilder hex = new StringBuilder(txt.length());
        for (final char chr : txt.toCharArray()) {
            if (chr == ' ') {
                continue;
            }
            hex.append(chr);
        }
        final String result;
        try {
            final byte[] bytes = Hex.decodeHex(String.valueOf(hex).toCharArray());
            result = new String(bytes, "UTF-8");
        } catch (final DecoderException | UnsupportedEncodingException exception) {
            throw new IllegalArgumentException(
                String.format("Invalid String %s, cannot unhex", txt),
                exception
            );
        }
        return result;
    }
}
