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

import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;
import org.apache.maven.plugins.annotations.Parameter;
import org.cactoos.scalar.Unchecked;
import org.eolang.maven.hash.CommitHash;
import org.eolang.maven.objectionary.Objectionary;

/**
 * {@link SafeMojo} with objectionaries.
 *
 * @since 0.29.6
 */
abstract class SafeMojoWithObjectionaries extends SafeMojo {
    /**
     * The Git hash to pull objects from, in objectionary.
     *
     * @since 0.21.0
     * @checkstyle VisibilityModifierCheck (6 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    @Parameter(property = "eo.tag", required = true, defaultValue = "master")
    protected String tag = "master";

    /**
     * Read hashes from local file.
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (6 lines)
     */
    @Parameter(property = "offlineHashFile")
    protected Path offlineHashFile;

    /**
     * Return hash by pattern.
     * -DofflineHash=0.*.*:abc2sd3
     * -DofflineHash=0.2.7:abc2sd3,0.2.8:s4se2fe
     *
     * @checkstyle MemberNameCheck (7 lines)
     * @checkstyle VisibilityModifierCheck (6 lines)
     */
    @Parameter(property = "offlineHash")
    protected String offlineHash;

    /**
     * The objectionary.
     *
     * @checkstyle VisibilityModifierCheck (5 lines)
     */
    @SuppressWarnings("PMD.ImmutableField")
    protected Objectionary objectionary;

    /**
     * Hash-Objectionary map.
     * @todo #1602:30min Use objectionaries to pull objects with different
     *  versions. Objects with different versions are stored in different
     *  storages (objectionaries). Every objectionary hash its own hash.
     *  To pull versioned object from objectionary firstly we need to get
     *  right objectionary by object's version and then get object from that
     *  objectionary by name.
     * @todo #1602:30min Use objectionaries to probe objects with different
     *  versions. Objects with different versions are stored in different
     *  storages (objectionaries). Every objectionary has its own hash.
     *  To get versioned object from objectionary firstly we need to get
     *  right objectionary by object's version and then get object from that
     *  objectionary by name.
     * @checkstyle MemberNameCheck (5 lines)
     * @checkstyle VisibilityModifierCheck (4 lines)
     */
    protected final Map<String, Objectionary> objectionaries = new HashMap<>();

    /**
     * Get objectionary from the map by given hash.
     * Put given objectionary to the map if absent.
     * @param hash Hash.
     * @param objry Objectionary as scalar to put if absent.
     * @return Objectionary by given hash.
     */
    protected Objectionary putIfAbsent(final CommitHash hash, final Unchecked<Objectionary> objry) {
        final String value = hash.value();
        if (!this.objectionaries.containsKey(value)) {
            this.objectionaries.put(value, objry.value());
        }
        return this.objectionaries.get(value);
    }
}
