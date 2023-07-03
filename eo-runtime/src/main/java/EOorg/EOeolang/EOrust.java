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

/*
 * @checkstyle PackageNameCheck (8 lines)
 */
package EOorg.EOeolang;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Base64;
import java.util.concurrent.ConcurrentHashMap;
import org.apache.commons.lang3.NotImplementedException;
import org.apache.commons.lang3.SystemUtils;
import org.cactoos.bytes.Base64Bytes;
import org.cactoos.bytes.BytesOf;
import org.cactoos.bytes.IoCheckedBytes;
import org.cactoos.scalar.IoChecked;
import org.cactoos.text.Base64Decoded;
import org.cactoos.text.IoCheckedText;
import org.cactoos.text.TextOf;
import org.eolang.AtComposite;
import org.eolang.AtFree;
import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.XmirObject;

/**
 * Rust.
 *
 * @since 0.29
 * @checkstyle MethodNameCheck (100 lines)
 * @checkstyle LineLengthCheck (100 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@XmirObject(oname = "rust")
public class EOrust extends PhDefault {

    /**
     * Map with location of the `code` attribute as the key
     * and native method as the value.
     */
    private static final ConcurrentHashMap<String, String> NAMES;

    static {
        try {
            NAMES = load("target/eo-test/names");
        } catch (final IOException exc) {
            throw new ExFailure(
                "Cannot read the file target/eo-test/names",
                exc
            );
        }
        final String lib;
        if (SystemUtils.IS_OS_WINDOWS) {
            lib = "common.dll";
        } else if (SystemUtils.IS_OS_LINUX) {
            lib = "libcommon.so";
        } else if (SystemUtils.IS_OS_MAC) {
            lib = "libcommon.dylib";
        } else {
            throw new NotImplementedException(
                String.format(
                    "Rust inserts are not supported by %s os. Only windows, linux and macos are allowed.",
                    System.getProperty("os.name")
                )
            );
        }
        final File libs = Paths.get("target")
            .resolve("eo-test")
            .resolve("Lib").toFile();
        if (libs.isDirectory()) {
            for (final File subdir: libs.listFiles()) {
                final Path path = subdir.toPath()
                    .resolve("target")
                    .resolve("debug")
                    .resolve(lib)
                    .toAbsolutePath();
                if (path.toFile().exists()){
                    System.load(path.toString());
                }
            }
        }
    }

    /**
     * Ctor.
     * @param sigma Sigma
     */
    public EOrust(final Phi sigma) {
        super(sigma);
        this.add("code", new AtFree());
        this.add("params", new AtFree());
        this.add(
            "φ",
            new AtComposite(
                this,
                rho -> {
                    final String name = NAMES.get(
                        rho.attr("code").get().locator().split(":")[0]
                    );
                    final Method method = Class.forName(
                        String.format(
                            "EOrust.natives.%s",
                            name
                        )
                    ).getDeclaredMethod(name, null);
                    return new Data.ToPhi(
                        Long.valueOf((int) method.invoke(null))
                    );
                }
            )
        );
    }

    /**
     * Loads names map.
     * @param src Where to load from.
     * @return Names map.
     * @throws IOException If any issues with IO.
     */
    private static ConcurrentHashMap<String, String> load(final String src) throws IOException {
        try (ObjectInputStream map = new ObjectInputStream(
            new ByteArrayInputStream(
                new IoCheckedBytes(
                    new Base64Bytes(
                        new BytesOf(
                            new TextOf(Paths.get(src))
                        )
                    )
                ).asBytes()
            )
        )) {
            final Object result = map.readObject();
            if (result.getClass() != ConcurrentHashMap.class) {
                throw new ClassCastException(
                    String.format(
                        "Object inside %s has wrong class %s",
                        src,
                        result.getClass()
                    )
                );
            }
            return (ConcurrentHashMap<String, String>) result;
        } catch (final ClassNotFoundException exc) {
            throw new IllegalArgumentException(
                String.format(
                    "File %s contains invalid data",
                    src
                ),
                exc
            );
        }
    }
}
