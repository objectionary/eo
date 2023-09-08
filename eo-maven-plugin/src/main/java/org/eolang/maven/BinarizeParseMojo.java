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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Hex;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.plugins.annotations.ResolutionScope;
import org.cactoos.map.MapOf;
import org.eolang.maven.footprint.FtDefault;
import org.eolang.maven.rust.Commented;
import org.eolang.maven.rust.Module;
import org.eolang.maven.rust.Names;
import org.eolang.maven.rust.Native;
import org.eolang.maven.rust.PrimeModule;
import org.eolang.maven.rust.Project;
import org.eolang.maven.tojos.ForeignTojo;
import org.eolang.maven.util.Home;
import org.eolang.parser.ParsingTrain;

/**
 * Parse rust inserts.
 *
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 * @since 0.1
 * @todo #2238:30min Specify directory for names via pom.xml.Now names map is
 *  serialized in targetDir.toPath().getParent() which is a bad decision since
 *  it must be created just as target/names.
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
     * The directory with generated .rs files.
     */
    public static final Path CODES = Paths.get("codes");

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

    /**
     * The directory with eo_env rust project.
     * @checkstyle MemberNameCheck (8 lines)
     */
    @Parameter(
        property = "eo.env",
        required = true,
        defaultValue = "${project.basedir}/src/main/rust/eo_env"
    )
    @SuppressWarnings("PMD.UnusedPrivateField")
    private File eoEnvDir;

    @Override
    public void exec() throws IOException {
        final Names names = new Names(targetDir.toPath().getParent());
        new File(this.targetDir.toPath().resolve("Lib/").toString()).mkdirs();
        for (final ForeignTojo tojo : this.scopedTojos().withOptimized()) {
            final Path file = tojo.optimized();
            final XML input = new XMLDocument(file);
            final List<XML> nodes = this.addRust(input).nodes("/program/rusts/rust");
            for (final XML node: nodes) {
                final String code = BinarizeParseMojo.unhex(node.xpath("@code").get(0));
                final List<String> dependencies =
                    node.xpath("./dependencies/dependency/attribute(name)")
                    .stream()
                    .map(BinarizeParseMojo::unhex)
                    .collect(Collectors.toList());
                final String function = names.name(
                    node.xpath("@code_loc").get(0)
                );
                final String filename = String.format(
                    "%s%s",
                    function,
                    ".rs"
                );
                final Path target = BinarizeMojo.DIR
                    .resolve(BinarizeParseMojo.CODES)
                    .resolve(filename);
                new Home(this.targetDir.toPath()).save(
                    code,
                    target
                );
                Logger.info(
                    this,
                    "Binarized %s from %s",
                    filename,
                    input.xpath("/program/@name").get(0)
                );
                new Project(this.targetDir.toPath().resolve("Lib/".concat(function)))
                    .with(new Module(code, "src/foo"), dependencies)
                    .with(new PrimeModule(function, "src/lib"), new ArrayList<>(1))
                    .dependency(
                        "eo_env",
                        new MapOf<>("path", this.eoEnvDir.getAbsolutePath())
                    )
                    .save();
                new Commented(new Native(function, "EOrust.natives"), "//")
                    .save(
                    new FtDefault(
                        this.generatedDir.toPath().resolve("EOrust").resolve("natives")
                    )
                );
            }
        }
        names.save();
    }

    /**
     * Creates a "rust" section in xml file and returns the resulting XML.
     * @param input The .xmir file
     * @return The content of rust section
     */
    private XML addRust(
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
