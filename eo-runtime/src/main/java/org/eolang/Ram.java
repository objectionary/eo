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
 * @checkstyle PackageNameCheck (4 lines)
 */
package org.eolang;

import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.UncheckedIOException;
import java.lang.management.ManagementFactory;
import java.nio.file.Files;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Random access.
 *
 * @since 0.19
 */
@Versionized
public enum Ram {
    /**
     * Ram instance.
     */
    INSTANCE;

    /**
     * Phi to File mapping.
     */
    private final Map<Phi, RandomAccessFile> addresses = new ConcurrentHashMap<>();

    /**
     * Read.
     * @param object Owner.
     * @param position Position.
     * @param length Length.
     * @return Byte array.
     * @throws IOException If fails.
     */
    public synchronized byte[] read(
        final Phi object,
        final int position,
        final int length
    ) throws IOException {
        final RandomAccessFile ram = this.init(object);
        ram.seek(position);
        final byte[] buffer = new byte[length];
        ram.readFully(buffer, 0, length);
        return buffer;
    }

    /**
     * Write.
     * @param object Owner.
     * @param position Position to write.
     * @param bytes Bytes to wite.
     * @throws IOException If fails.
     */
    public synchronized void write(
        final Phi object,
        final int position,
        final byte[] bytes
    ) throws IOException {
        final RandomAccessFile buffer = this.init(object);
        buffer.seek(position);
        buffer.write(bytes);
    }

    /**
     * Initialize storage.
     * @param phi Owner.
     * @return Storage file
     */
    private RandomAccessFile init(final Phi phi) {
        final long size = new Dataized(phi.attr("size").get()).take(Long.class);
        return this.addresses.computeIfAbsent(
            phi,
            o -> {
                try {
                    final RandomAccessFile file = new RandomAccessFile(
                        Files.createTempFile(
                            ManagementFactory
                                .getRuntimeMXBean()
                                .getName(),
                            ".mem"
                        ).toFile(),
                        "rws"
                    );
                    file.setLength(size);
                    return file;
                } catch (final IOException ex) {
                    throw new UncheckedIOException(ex);
                }
            }
        );
    }
}
