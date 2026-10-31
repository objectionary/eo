/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Collection;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;
import org.cactoos.Scalar;
import org.cactoos.scalar.Sticky;
import org.cactoos.scalar.Synced;
import org.cactoos.scalar.Unchecked;

/**
 * The rows of the inference tables, indexed by the locator each one is about.
 *
 * <p>{@code purify.xsl} reads {@code answers.xml} and {@code facts.xml} by
 * locator only, so a transpile of one file reads that file's rows (#7945):
 * the rows {@code answers.xml} has for the places of the file, and the rows
 * the {@code objects} of {@code facts.xml} have for its formations.</p>
 *
 * <p>Both tables go into the digest. The rows of {@code answers.xml} change
 * with any file of the program, since what a place turns out to be depends
 * on its callers. The rows of {@code facts.xml} mostly follow the text of the
 * file, which the cache key already holds, though whether a row calls a
 * formation bare depends on what other files put into its package. With both
 * tables in, nobody has to work out which of the rows the stylesheet reads
 * can change while the file stays the same. The price is an extra transpile
 * when a row changes that the stylesheet does not read.</p>
 *
 * <p>The reading is remembered under a lock, because a parallel transpile
 * asks one of these from every worker at once. A {@link Sticky} on its own
 * keeps the answer in a map nobody guards, so a worker arriving while
 * another was still putting the answer there was handed nothing and fell
 * over on it (#8324).</p>
 *
 * @since 0.75.0
 */
final class Rows {

    /**
     * The tables, by the locator of the object each row is about.
     */
    private final Scalar<SortedMap<String, String>> index;

    /**
     * Ctor.
     *
     * @param tables The directory with the tables of {@link MjInference}
     */
    Rows(final Path tables) {
        this.index = new Synced<>(
            new Sticky<>(
                () -> {
                    final SortedMap<String, String> rows = new TreeMap<>();
                    Rows.merge(rows, tables.resolve("answers.xml"), "/answers/o", "@loc");
                    Rows.merge(rows, tables.resolve("facts.xml"), "/facts/objects/type", "@id");
                    return rows;
                }
            )
        );
    }

    /**
     * Digest of the rows these objects and what they hold are named in.
     * Absent tables and empty ones answer alike.
     *
     * @param locators The locators of the objects a file holds
     * @return Twelve hex characters
     */
    String digest(final Collection<String> locators) {
        final SortedMap<String, String> rows = new Unchecked<>(this.index).value();
        try {
            final MessageDigest sha = MessageDigest.getInstance("SHA-256");
            for (final String locator : new TreeSet<>(locators)) {
                final String prefix = String.format("%s.", locator);
                final SortedMap<String, String> subtree = rows.subMap(
                    locator, String.format("%s%c", prefix, Character.MAX_VALUE)
                );
                for (final Map.Entry<String, String> row : subtree.entrySet()) {
                    if (!row.getKey().equals(locator) && !row.getKey().startsWith(prefix)) {
                        continue;
                    }
                    Rows.feed(sha, row.getKey());
                    Rows.feed(sha, row.getValue());
                }
            }
            return String.format("%064x", new BigInteger(1, sha.digest())).substring(0, 12);
        } catch (final NoSuchAlgorithmException ex) {
            throw new IllegalStateException("SHA-256 is not available", ex);
        }
    }

    private static void merge(
        final SortedMap<String, String> rows, final Path table, final String path,
        final String key
    ) throws IOException {
        if (Files.exists(table)) {
            for (final XML row : new XMLDocument(table).nodes(path)) {
                rows.merge(
                    row.xpath(key).get(0),
                    String.format("%s:%s", table.getFileName(), row),
                    String::concat
                );
            }
        }
    }

    private static void feed(final MessageDigest digest, final String value) {
        final byte[] bytes = value.getBytes(StandardCharsets.UTF_8);
        digest.update(
            String.format("%d\0", bytes.length).getBytes(StandardCharsets.UTF_8)
        );
        digest.update(bytes);
    }
}
