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

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import com.yegor256.xsline.Shift;
import com.yegor256.xsline.TrBulk;
import com.yegor256.xsline.TrClasspath;
import com.yegor256.xsline.Train;
import com.yegor256.xsline.Xsline;
import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.eolang.maven.rust.FFINode;
import org.eolang.maven.rust.Names;
import org.eolang.maven.rust.RustNode;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.parser.ParsingTrain;

/**
 * Parse rust inserts.
 *
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 * @since 0.1
 */
@Mojo(
    name = "binarize_parse",
    defaultPhase = LifecyclePhase.PROCESS_SOURCES,
    threadSafe = true,
    requiresDependencyResolution = ResolutionScope.COMPILE
)

@SuppressWarnings("PMD.LongVariable")
public final class BinarizeParseMojo extends SafeMojo {

    /**
     * The directory where to binarize to.
     */
    public static final Path DIR = Paths.get("binarize");

    /**
     * Parsing train with XSLs. The task of XSLs is to find all the FFI inserts and put them at
     * the end of the xmir file. When adding a new language for FFI inserts, you need to add the
     * appropriate XSL transformation.
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

    /**
     * The directory with portal project.
     * @checkstyle MemberNameCheck (8 lines)
     */
    @Parameter(
        property = "eo.portal",
        required = true,
        defaultValue = "${project.basedir}/src/main/rust/eo"
    )
    @SuppressWarnings("PMD.UnusedPrivateField")
    private File eoPortalDir;

    /**
     * To uniquely name different ffi inerts.
     */
    private Names names;

    @Override
    public void exec() throws IOException {
        new File(this.targetDir.toPath().resolve("Lib/").toString()).mkdirs();
        for (final ForeignTojo tojo : this.scopedTojos().withOptimized()) {
            final Path file = tojo.shaken();
            this.getFFIs(new XMLDocument(file))
                .forEach(FFINode::generateUnchecked);
        }
        this.names.save();
    }

    /**
     * Creates sections for each language for FFI insert in xmir and returns the resulting XML file.
     * @param input The .xmir file
     * @return The content of FFI inserts sections
     */
    private XML addFFIs(
        final XML input
    ) {
        final String name = input.xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Train<Shift> trn = new SpyTrain(
            BinarizeParseMojo.TRAIN,
            place.make(this.targetDir.toPath().resolve(BinarizeMojo.DIR), "")
        );
        return new Xsline(trn).pass(input);
    }

    /**
     * Add ffi node via xsl transformation and return list of them.
     * @param input Input xmir.
     * @return FFI nodes.
     * @todo #2649:90min This method may be more general. We need to get rid from rust dependencies
     *  in this method, because when adding another type of inserts it will be just copy-paste here.
     *  First of all, the for-loop must create all kinds of FFI nodes, not only {@link RustNode}. I
     *  think we can implement it, using something like {@code FFINodeBuilder}, that will return
     *  appropriate FFI node for every XML node from {@code nodes}. Also it will be great to move
     *  paths to XML FFI insert nodes (such as {@code "/program/rusts/rust"}) from this method to
     *  a class field.
     * @checkstyle AbbreviationAsWordInNameCheck (8 lines)
     */
    private Collection<FFINode> getFFIs(final XML input) {
        final List<XML> nodes = this.addFFIs(input).nodes("/program/rusts/rust");
        final Collection<FFINode> ret = new ArrayList<>(nodes.size());
        for (final XML node : nodes) {
            ret.add(
                new RustNode(
                    node,
                    this.names,
                    this.targetDir.toPath().resolve("Lib"),
                    this.eoPortalDir.toPath(),
                    this.generatedDir.toPath().resolve("EOrust").resolve("natives")
                )
            );
        }
        return ret;
    }

}
