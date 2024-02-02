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

/*
 * @checkstyle PackageNameCheck (8 lines)
 */
package EOorg.EOeolang;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import org.eolang.AtFree;
import org.eolang.Atom;
import org.eolang.Data;
import org.eolang.ExFailure;
import org.eolang.ExNative;
import org.eolang.PhDefault;
import org.eolang.Phi;
import org.eolang.Universe;
import org.eolang.UniverseDefault;
import org.eolang.UniverseSafe;
import org.eolang.Versionized;
import org.eolang.XmirObject;

/**
 * Rust.
 *
 * @since 0.29
 * @checkstyle MethodNameCheck (100 lines)
 * @checkstyle LineLengthCheck (100 lines)
 * @checkstyle TypeNameCheck (5 lines)
 */
@Versionized
@XmirObject(oname = "rust")
public final class EOrust extends PhDefault implements Atom {

    /**
     * Map with location of the `code` attribute as the key
     * and native method as the value.
     */
    private static final ConcurrentHashMap<String, String> NAMES;

    /**
     * All phis indexed while executing of native method.
     */
    private final Map<Integer, Phi> phis = new HashMap<>();

    /**
     * Error that possibly was thrown while the native function execution.
     */
    private final AtomicReference<Throwable> error = new AtomicReference<>();

    static {
        try {
            NAMES = load("target/names");
        } catch (final IOException exc) {
            throw new ExFailure(
                "Cannot read the file target/eo-test/names",
                exc
            );
        }
        final String lib;
        final String system = System.getProperty("os.name").toLowerCase();
        if (system.contains("win")) {
            lib = "common.dll";
        } else if (system.contains("nix") || system.contains("nux") || system.contains("aix")) {
            lib = "libcommon.so";
        } else if (system.contains("mac")) {
            lib = "libcommon.dylib";
        } else {
            throw new UnsupportedOperationException(
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
            for (final File subdir : libs.listFiles()) {
                final Path path = subdir.toPath()
                    .resolve("target")
                    .resolve("debug")
                    .resolve(lib)
                    .toAbsolutePath();
                if (path.toFile().exists()) {
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
        this.add("portal", new AtFree());
        this.add("params", new AtFree());
    }

    @Override
    public Phi lambda() throws Exception {
        final String name = NAMES.get(
            this.attr("code").get().locator().split(":")[0]
        );
        final Method method = Class.forName(
            String.format(
                "EOrust.natives.%s",
                name
            )
        ).getDeclaredMethod(name, Universe.class);
        if (method.getReturnType() != byte[].class) {
            throw new ExFailure(
                "Return type of %s is %s, required %s",
                method,
                method.getReturnType(),
                byte[].class
            );
        }
        final Phi portal = this.attr("portal").get();
        return this.translate(
            (byte[]) method.invoke(
                null,
                new UniverseSafe(
                    new UniverseDefault(
                        portal, this.phis
                    ),
                    this.error
                )
            ),
            this.attr("code").get().locator()
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
                Base64.getDecoder().decode(
                    Files.readAllBytes(Paths.get(src))
                )
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

    /**
     * Translates byte message from rust side to Phi object.
     * @param message Message that native method returns.
     * @param insert Location of the rust insert.
     * @return Phi object.
     */
    private Phi translate(final byte[] message, final String insert) {
        final byte determinant = message[0];
        final byte[] content = Arrays.copyOfRange(message, 1, message.length);
        final Phi ret;
        final ByteBuffer buffer;
        switch (determinant) {
            case 0:
                buffer = ByteBuffer.allocate(Integer.BYTES);
                buffer.put(content);
                buffer.flip();
                final int vertex = buffer.getInt();
                ret = this.phis.get(vertex);
                if (ret == null) {
                    throw new ExFailure(
                        String.format(
                            "Returned phi with vertex %d (%s in bytes) was not indexed",
                            vertex,
                            Arrays.toString(content)
                        )
                    );
                }
                break;
            case 1:
                buffer = ByteBuffer.allocate(Double.BYTES);
                buffer.put(content);
                buffer.flip();
                ret = new Data.ToPhi(buffer.getDouble());
                break;
            case 2:
                buffer = ByteBuffer.allocate(Long.BYTES);
                buffer.put(content);
                buffer.flip();
                ret = new Data.ToPhi(buffer.getLong());
                break;
            case 4:
                ret = new Data.ToPhi(content);
                break;
            case 3:
                ret = new Data.ToPhi(
                    new String(content, StandardCharsets.UTF_8)
                );
                break;
            case 5:
                final String cause = new String(content, StandardCharsets.UTF_8);
                if (this.error.get() == null) {
                    throw new ExNative(
                        "Rust insert failed in %s with message '%s'",
                        insert,
                        cause
                    );
                } else {
                    throw new ExNative(
                        String.format(
                            "Rust insert failed in %s with message '%s'",
                            insert,
                            cause
                        ),
                        this.error.get()
                    );
                }
            default:
                throw new ExNative(
                    "Returning Strings and raw bytes is not implemented yet, insert %s",
                    insert
                );
        }
        return ret;
    }
}
