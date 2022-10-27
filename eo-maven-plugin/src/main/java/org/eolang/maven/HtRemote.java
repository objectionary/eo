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
import java.util.Scanner;

/**
 * Hash of tag.
 * @since 0.26
 */
final class HtRemote implements HashOfTag {

    /**
     * Cached map of hashes.
     */
    private static final Map<String, String> CACHE = HtRemote.safeLoad();

    /**
     * The URL where the list is kept.
     */
    private static final String HOME = "https://home.objectionary.com/tags.txt";

    /**
     * Tag.
     */
    private final String tag;

    /**
     * Constructor.
     * @param hash Tag
     */
    HtRemote(final String hash) {
        this.tag = hash;
    }

    /**
     * Hash of tag.
     * @return SHA of commit
     */
    @Override
    public String hash() {
        final String result = HtRemote.CACHE.get(this.tag);
        if (result == null) {
            throw new IllegalArgumentException(
                String.format(
                    "Tag '%s' doesn't exist or the list of all tags was not loaded correctly",
                    this.tag
                )
            );
        }
        Logger.debug(this, "Git sha of %s is %s", this.tag, result);
        return result;
    }

    /**
     * Load all hashes and tags.
     * @return Map of them (hash -> tag)
     */
    private static Map<String, String> safeLoad() {
        Map<String, String> map;
        try {
            map = HtRemote.load();
        } catch (final IOException ex) {
            Logger.warn(
                HtRemote.class,
                "Failed to load catalog of Git hashes from %s, because of %s: '%s'",
                HtRemote.HOME, ex.getClass().getSimpleName(), ex.getMessage()
            );
            map = new HashMap<>(0);
        }
        return map;
    }

    /**
     * Load all hashes and tags.
     * @return Map of them (hash -> tag)
     * @throws IOException if fails
     */
    private static Map<String, String> load() throws IOException {
        final InputStream ins = new URL(HtRemote.HOME).openStream();
        try (Scanner scanner = new Scanner(ins)) {
            final Map<String, String> map = new HashMap<>(0);
            while (scanner.hasNextLine()) {
                final String line = scanner.nextLine();
                final String[] parts = line.split("\t");
                map.put(parts[1], parts[0]);
            }
            return map;
        }
    }

}
