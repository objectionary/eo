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
package EOorg.EOeolang.EOfs;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import org.eolang.ExFailure;
import org.eolang.Versionized;

/**
 * File streams.
 * @since 0.40
 */
@Versionized
final class Files {
    /**
     * Files instance
     */
    static final Files INSTANCE = new Files();

    /**
     * File input streams for reading.
     */
    private final ConcurrentHashMap<String, Object[]> streams = new ConcurrentHashMap<>(0);

    /**
     * Ctor.
     */
    private Files() {
        // singleton :(
    }

    /**
     * Open file for reading and writing.
     * @param name Name of the file
     * @throws FileNotFoundException If can't open file
     */
    void open(final String name) throws FileNotFoundException {
        this.streams.putIfAbsent(name, new Object[] {
            new FileInputStream(name),
            new FileOutputStream(name, true)
        });
    }

    /**
     * Read given amount of bytes from file.
     * @param name Name of file
     * @param size Amount of bytes to read
     * @return Read bytes
     * @throws IOException If fails to read
     */
    byte[] read(final String name, final int size) throws IOException {
        synchronized (this.streams) {
            if (!this.streams.containsKey(name)) {
                throw new ExFailure(
                    "File input stream with name %s is absent, can't read",
                    name
                );
            }
            final byte[] read = new byte[size];
            int character;
            int processed = 0;
            final InputStream input = (InputStream) this.streams.get(name)[0];
            while (processed < size && (character = input.read()) != -1) {
                read[processed] = (byte) character;
                ++processed;
            }
            return Arrays.copyOf(read, processed);
        }
    }

    /**
     * Write given byte buffer to file.
     * @param name Name of the file
     * @param buffer Byte buffer to write
     * @throws IOException If fails to write
     */
    void write(final String name, final byte[] buffer) throws IOException {
        synchronized (this.streams) {
            if (!this.streams.containsKey(name)) {
                throw new ExFailure(
                    "File output stream with name %s is absent, can't read",
                    name
                );
            }
            ((OutputStream) this.streams.get(name)[1]).write(buffer);
        }
    }

    /**
     * Close files streams.
     * @param name File name
     * @throws IOException If fails to close the streams
     */
    void close(final String name) throws IOException {
        synchronized (this.streams) {
            if (!this.streams.containsKey(name)) {
                throw new ExFailure(
                    "File streams with name %s is absent, can't close",
                    name
                );
            }
            ((InputStream) this.streams.get(name)[0]).close();
            ((OutputStream) this.streams.get(name)[1]).close();
            this.streams.remove(name);
        }
    }
}
