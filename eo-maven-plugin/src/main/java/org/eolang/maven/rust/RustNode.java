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
package org.eolang.maven.rust;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Hex;
import org.cactoos.map.MapOf;
import org.eolang.maven.footprint.FtDefault;

/**
 * {@link FFINode} for Rust inserts.
 * @since 0.34
 */
public final class RustNode implements FFINode {

    /**
     * Corresponding node from xmir.
     */
    private final XML node;

    /**
     * To name the insert according to its location.
     */
    private final Names names;

    /**
     * Where is Lib directory- the directory with cargo projects.
     *  Usually it is target/Lib.
     */
    private final Path lib;

    /**
     * Path to portal dependency- rust library with eo handlers.
     */
    private final Path portal;

    /**
     * Path to java generated sources. Usually target/generated-sources
     */
    private final Path generated;

    /**
     * Ctor.
     * @param node Node.
     * @param names Names.
     * @param lib Lib directory.
     * @param portal Portal directory.
     * @param generated Generated directory.
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    public RustNode(
        final XML node, final Names names, final Path lib, final Path portal, final Path generated
    ) {
        this.node = node;
        this.names = names;
        this.lib = lib;
        this.portal = portal;
        this.generated = generated;
    }

    @Override
    public void generate() throws IOException {
        final String code = RustNode.unhex(this.node.xpath("@code").get(0));
        final List<String> dependencies =
            this.node.xpath("./dependencies/dependency/attribute(name)")
                .stream()
                .map(RustNode::unhex)
                .collect(Collectors.toList());
        final String function = this.names.name(
            this.node.xpath("@code_loc").get(0)
        );
        final String filename = String.format(
            "%s%s",
            function,
            ".rs"
        );
        new Project(this.lib.resolve(function))
            .with(new Module(code, "src/foo"), dependencies)
            .with(new PrimeModule(function, "src/lib"), new ArrayList<>(1))
            .dependency(
                "eo",
                new MapOf<>("path", this.portal.toAbsolutePath().toString())
            )
            .save();
        Logger.info(
            this,
            "Created cargo project %s from %s",
            filename,
            this.node.xpath("@code_loc").get(0)
        );
        new Commented(
            new Native(function, "EOrust.natives"),
            "//"
        ).save(new FtDefault(this.generated));
        Logger.info(
            this,
            "Created java class %s from %s",
            filename,
            this.node.xpath("@code_loc").get(0)
        );
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
            result = new String(bytes, StandardCharsets.UTF_8);
        } catch (final DecoderException exception) {
            throw new IllegalArgumentException(
                String.format("Invalid String %s, cannot unhex", txt),
                exception
            );
        }
        return result;
    }
}
