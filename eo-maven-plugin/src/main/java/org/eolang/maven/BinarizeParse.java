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
import java.util.Map;
import java.util.function.BiFunction;
import org.cactoos.map.MapOf;
import org.eolang.maven.rust.Buildable;
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
@SuppressWarnings("PMD.LongVariable")
public final class BinarizeParse {

    /**
     * The directory where to binarize to.
     */
    public static final Path DIR = Paths.get("binarize");

    /**
     * Parsing train with XSLs. The task of XSLs is to find all the FFI inserts and put them at
     * the end of the xmir file. When adding a new language for FFI inserts, you need to add the
     * appropriate XSL transformation.
     */
    private static final Train<Shift> TRAIN = new TrBulk<>(
        new TrClasspath<>(
            new ParsingTrain()
                .empty()
        ),
        Arrays.asList(
            "/org/eolang/maven/add_rust/add_rust.xsl"
        )
    ).back().back();

    /**
     * Map that matches ffi insert xpath to building of FFINode.
     */
    private static final
        Map<String, BiFunction<XML, BinarizeParse, FFINode>> FACTORY = new MapOf<>(
        "/program/rusts/rust",
            (node, mojo) -> new RustNode(
            node,
            mojo.names,
            mojo.targetDir.toPath().resolve("Lib"),
            mojo.eoPortalDir.toPath(),
            mojo.generatedDir.toPath().resolve("EOrust").resolve("natives")
        )
    );

    /**
     * Target directory.
     * @checkstyle MemberNameCheck (7 lines)
     */
    private final File generatedDir;

    /**
     * The directory with portal project.
     * @checkstyle MemberNameCheck (8 lines)
     */
    private final File eoPortalDir;

    /**
     * To uniquely name different ffi inerts.
     */
    private final Names names;

    /**
     * To uniquely name different ffi inerts.
     * @checkstyle MemberNameCheck (7 lines)
     */
    private final File targetDir;

    /**
     * Ctor.
     * @param generated Generated directory.
     * @param portal Directory to portal dependency.
     * @param names Names.
     * @param target Target directory.
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    public BinarizeParse(
        final File generated,
        final File portal,
        final Names names,
        final File target
    ) {
        this.generatedDir = generated;
        this.eoPortalDir = portal;
        this.names = names;
        this.targetDir = target;
    }

    /**
     * Parse ffi nodes in tojos.
     * @param tojos Tojos where to parse ffi node,
     * @return Collection of {@link FFINode}s that will be then also compiled.
     * @throws IOException if any issue with IO.
     */
    public Collection<Buildable> exec(final Collection<ForeignTojo> tojos) throws IOException {
        final Collection<Buildable> res = new ArrayList<>(0);
        new File(this.targetDir.toPath().resolve("Lib/").toString()).mkdirs();
        for (final ForeignTojo tojo : tojos) {
            final Path file = tojo.verified();
            for (final FFINode ffi: this.getFFIs(new XMLDocument(file))) {
                ffi.generateUnchecked();
                if (ffi instanceof Buildable) {
                    res.add((Buildable) ffi);
                }
            }
        }
        this.names.save();
        return res;
    }

    /**
     * Creates sections for each language for FFI insert in xmir and returns the resulting XML file.
     * @param input The .xmir file
     * @return The content of FFI inserts sections
     * @checkstyle AbbreviationAsWordInNameCheck (8 lines)
     */
    private XML addFFIs(
        final XML input
    ) {
        final String name = input.xpath("/program/@name").get(0);
        final Place place = new Place(name);
        final Train<Shift> trn = new SpyTrain(
            BinarizeParse.TRAIN,
            place.make(this.targetDir.toPath().resolve(BinarizeMojo.DIR), "")
        );
        return new Xsline(trn).pass(input);
    }

    /**
     * Add ffi node via xsl transformation and return list of them.
     * @param input Input xmir.
     * @return FFI nodes.
     * @checkstyle AbbreviationAsWordInNameCheck (8 lines)
     */
    private Collection<FFINode> getFFIs(final XML input) {
        final Collection<FFINode> ret = new ArrayList<>(0);
        final XML injected = this.addFFIs(input);
        BinarizeParse.FACTORY.forEach(
            (xpath, ctor) -> injected
                .nodes(xpath)
                .forEach(node -> ret.add(ctor.apply(node, this)))
        );
        return ret;
    }

}
