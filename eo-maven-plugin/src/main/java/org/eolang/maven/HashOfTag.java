/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016-2022 Objectionary.com
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
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Scanner;

/**
 * Hash of tag.
 * @since 0.26
 */
final class HashOfTag {

    /**
     * Static map with all hash requests.
     */
    private static final Map<String, String> REQUESTED_HASHES = new HashMap<>();

    /**
     * Tag.
     */
    private final String tag;

    /**
     * Constructor.
     * @param hash Tag
     */
    HashOfTag(final String hash) {
        this.tag = hash;
    }

    /**
     * Hash of tag.
     * The method is thread-safe.
     * @return SHA of commit
     * @throws IOException if fails
     */
    public String hash() throws IOException {
        final String link = "https://home.objectionary.com/tags.txt";
        String result = "";
        synchronized (HashOfTag.REQUESTED_HASHES) {
            if (REQUESTED_HASHES.containsKey(this.tag)) {
                result = REQUESTED_HASHES.get(this.tag);
            } else {
                final InputStream ins = new URL(link).openStream();
                final Scanner scanner = new Scanner(ins);
                while (scanner.hasNextLine()) {
                    final String line = scanner.nextLine();
                    final String[] parts = line.split("\t");
                    if (Objects.equals(parts[1], this.tag)) {
                        result = parts[0];
                        REQUESTED_HASHES.put(this.tag, result);
                        break;
                    }
                }
            }
        }
        if (result.isEmpty()) {
            throw new IllegalArgumentException(
                String.format("Tag %s doesn't exist in %s", this.tag, link)
            );
        }
        Logger.info(this, "Git sha of %s is %s", this.tag, result);
        return result;
    }

}
