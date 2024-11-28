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
package org.eolang.maven.rust;

import com.jcabi.log.Logger;
import com.jcabi.xml.XML;
import com.yegor256.Jaxec;
import com.yegor256.Result;
import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.apache.commons.codec.DecoderException;
import org.apache.commons.codec.binary.Hex;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.cactoos.map.MapOf;
import org.cactoos.text.TextOf;
import org.cactoos.text.UncheckedText;

/**
 * {@link FFINode} for Rust inserts.
 * @since 0.34
 */
public final class RustNode implements Buildable {

    /**
     * Name of executable file which is result of cargo building.
     */
    public static final String LIB = RustNode.common();

    /**
     * Corresponding node from xmir.
     */
    private final XML node;

    /**
     * To name the insert according to its location.
     */
    private final String name;

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
        this(
            node,
            names.name(node.xpath("@code_loc").get(0)),
            lib,
            portal,
            generated
        );
    }

    /**
     * Main constructor.
     * @param node Node.
     * @param name Name.
     * @param lib Lib directory.
     * @param portal Portal directory.
     * @param generated Generated directory.
     * @checkstyle ParameterNumberCheck (10 lines)
     */
    public RustNode(
        final XML node, final String name, final Path lib, final Path portal, final Path generated
    ) {
        this.node = node;
        this.name = name;
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
        final String filename = String.format(
            "%s%s",
            this.name,
            ".rs"
        );
        new Project(this.lib.resolve(this.name))
            .with(new Module(code, "src/foo"), dependencies)
            .with(new PrimeModule(this.name, "src/lib"), new ArrayList<>(1))
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
            new Native(this.name, "EOrust.natives"),
            "//"
        ).save(this.generated);
        Logger.info(
            this,
            "Created java class %s from %s",
            filename,
            this.node.xpath("@code_loc").get(0)
        );
    }

    @Override
    public void build(final Path cache)  {
        try {
            this.buildChecked(cache);
        } catch (final IOException exception) {
            throw new UncheckedIOException(exception);
        }
    }

    /**
     * Build the project.
     * @param cache Cache directory.
     * @throws IOException If any issues with IO.
     */
    private void buildChecked(final Path cache) throws IOException {
        final File project = this.lib.resolve(this.name).toFile();
        final File cached = cache
            .resolve("Lib")
            .resolve(this.name)
            .resolve("target").toFile();
        if (RustNode.sameProject(
            project.toPath(),
            cache
                .resolve("Lib")
                .resolve(this.name)
        )) {
            Logger.info(
                this,
                "content of %s was not changed since the last launch",
                this.name
            );
            final File executable = cached.toPath()
                .resolve("debug")
                .resolve(RustNode.LIB)
                .toFile();
            if (executable.exists()) {
                FileUtils.copyFile(
                    executable,
                    project.toPath()
                        .resolve("target")
                        .resolve("debug")
                        .resolve(RustNode.LIB)
                        .toFile()
                );
            }
        } else {
            final File target = project.toPath().resolve("target").toFile();
            if (cached.exists()) {
                Logger.info(this, "Copying %s to %s", cached, target);
                FileUtils.copyDirectory(cached, target);
            }
            Logger.debug(this, "Building %s rust project..", project.getName());
            final Result res = new Jaxec("cargo", "build")
                .withCheck(false)
                .withRedirect(true)
                .withHome(project)
                .execUnsafe();
            if (res.code() != 0) {
                throw new BuildFailureException(
                    String.format(
                        "Failed to build cargo project in %s (#%d):\n%s",
                        project, res.code(), res.stdout()
                    )
                );
            }
            Logger.info(
                this,
                "Cargo building succeeded, update cached %s with %s",
                cached,
                target
            );
            FileUtils.copyDirectory(target.getParentFile(), cached.getParentFile());
        }
    }

    /**
     * Makes a text from Hexed text.
     * @param txt Hexed chars separated by backspace.
     * @return Normal text.
     */
    private static String unhex(final String txt) {
        final StringBuilder hex = new StringBuilder(txt.length());
        for (final char chr : txt.toCharArray()) {
            if (chr == '-') {
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

    /**
     * Check if the project was not changed.
     * @param src Directory in current target.
     * @param cached Directory in cache.
     * @return True if the project is the same.
     */
    private static boolean sameProject(final Path src, final Path cached) {
        return RustNode.sameFile(
            src.resolve("src/foo.rs"), cached.resolve("src/foo.rs")
        ) && RustNode.sameFile(
            src.resolve("src/lib.rs"), cached.resolve("src/lib.rs")
        ) && RustNode.sameFile(
            src.resolve("Cargo.toml"), cached.resolve("Cargo.toml")
        );
    }

    /**
     * Check if the source file is the same as in cache.
     * @param src Source file.
     * @param cached Cache file.
     * @return True if the same.
     */
    private static boolean sameFile(final Path src, final Path cached) {
        return cached.toFile().exists() && RustNode.uncomment(
            new UncheckedText(
                new TextOf(src)
            ).asString()
        ).equals(
            RustNode.uncomment(
                new UncheckedText(
                    new TextOf(cached)
                ).asString()
            )
        );
    }

    /**
     * Removed the first line from the string.
     * We need it because generated files are disclaimed.
     * @param content Content.
     * @return String without the first line.
     * @checkstyle StringLiteralsConcatenationCheck (8 lines)
     */
    private static String uncomment(final String content) {
        return content.substring(
            1 + content.indexOf(System.getProperty("line.separator"))
        );
    }

    /**
     * Calculates name for Rust shared library depending on OS.
     * @return Name.
     */
    private static String common() {
        final String result;
        if (SystemUtils.IS_OS_WINDOWS) {
            result = "common.dll";
        } else if (SystemUtils.IS_OS_LINUX) {
            result = "libcommon.so";
        } else if (SystemUtils.IS_OS_MAC) {
            result = "libcommon.dylib";
        } else {
            throw new IllegalArgumentException(
                String.format(
                    "Rust inserts are not supported in %s os. Only windows, linux and macos are allowed.",
                    System.getProperty("os.name")
                )
            );
        }
        return result;
    }
}
