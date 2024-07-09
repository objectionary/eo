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

import com.jcabi.log.Logger;
import com.yegor256.Jaxec;
import java.nio.file.Path;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.SystemUtils;
import org.eolang.maven.util.CJniInfo;

/**
 * Compiles C native libraries.
 *
 * @since 0.38
 */
public class NativeCLib {
    /**
     * The single C source for building shared library.
     */
    private final Path source;

    /**
     * The path to the target file where the resulting native library will be compiled.
     */
    private final Path target;

    /**
     * Ctor.
     * @param source Path to C source of native function.
     * @param path Path to the directory where compiled native library will be placed.
     */
    public NativeCLib(final Path source, final Path path) {
        this.source = source;
        this.target = path.resolve(NativeCLib.libName(source.getFileName().toString()));
    }

    /**
     * Compiles C native libraries.
     * @return Path to the target file where the resulting native library will be compiled.
     */
    public Path compile() {
        final String ccompiler = System.getenv("CC");
        if (ccompiler == null) {
            throw new IllegalStateException(
                "There is no environment variable \"CC\" with the proper specified C compiler path"
            );
        }
        try {
            Logger.debug(
                this,
                new Jaxec(
                    ccompiler,
                    String.format("-I%s", CJniInfo.COMMON_HEADER),
                    String.format("-I%s", CJniInfo.OS_SPEC_HEADER),
                    this.source.toString(),
                    "-shared",
                    "-o",
                    this.target.toString()
                ).withCheck(false).exec()
            );
        } catch (final IllegalArgumentException ex) {
            throw new IllegalArgumentException(
                "An error occurred while compiling the source code of the C native library",
                ex
            );
        }
        return this.target;
    }

    /**
     * Builds platform specific dynamic library name for given source.
     * @param source Source.
     * @return Platform-specific dynamic library name.
     */
    private static String libName(final String source) {
        final String raw = FilenameUtils.removeExtension(source);
        final String lib;
        if (SystemUtils.IS_OS_WINDOWS) {
            lib = String.format("%s.dll", raw);
        } else if (SystemUtils.IS_OS_LINUX) {
            lib = String.format("lib%s.so", raw);
        } else if (SystemUtils.IS_OS_MAC) {
            lib = String.format("lib%s.dylib", raw);
        } else {
            throw new UnsupportedOperationException(
                String.format(
                    "Native C libraries are not supported by %s os. Only windows, linux and macos are allowed.",
                    System.getProperty("os.name")
                )
            );
        }
        return lib;
    }
}
