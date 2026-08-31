/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.xml.XML;
import com.jcabi.xml.XMLDocument;
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
import org.cactoos.scalar.Unchecked;

/**
 * The rows of the inference tables, indexed by the locator each one is about.
 *
 * <p>{@code purify.xsl} reads {@code provides.xml} and {@code links.xml} by
 * locator only, so a transpile of one file reads that file's rows (#7945).</p>
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
     * @param tables The directory with the tables of {@link MjInference}
     */
    Rows(final Path tables) {
        this.index = new Sticky<>(
            () -> {
                final SortedMap<String, String> rows = new TreeMap<>();
                for (final String name : new String[] {"provides.xml", "links.xml"}) {
                    final Path table = tables.resolve(name);
                    if (Files.exists(table)) {
                        for (final XML row : new XMLDocument(table).nodes("/*/type[@id]")) {
                            rows.merge(
                                row.xpath("@id").get(0),
                                String.format("%s:%s", name, row),
                                String::concat
                            );
                        }
                    }
                }
                return rows;
            }
        );
    }

    /**
     * Digest of the rows these objects and what they hold are named in.
     * Absent tables and empty ones answer alike.
     * @param locators The locators of the objects a file holds
     * @return Twelve hex characters
     */
    String digest(final Collection<String> locators) {
        final SortedMap<String, String> rows = new Unchecked<>(this.index).value();
        try {
            final MessageDigest sha = MessageDigest.getInstance("SHA-256");
            for (final String locator : new TreeSet<>(locators)) {
                for (final Map.Entry<String, String> row : rows.tailMap(locator).entrySet()) {
                    if (!row.getKey().equals(locator)
                        && !row.getKey().startsWith(String.format("%s.", locator))) {
                        break;
                    }
                    sha.update(row.getKey().getBytes(StandardCharsets.UTF_8));
                    sha.update(row.getValue().getBytes(StandardCharsets.UTF_8));
                }
            }
            return String.format("%064x", new BigInteger(1, sha.digest())).substring(0, 12);
        } catch (final NoSuchAlgorithmException ex) {
            throw new IllegalStateException("SHA-256 is not available", ex);
        }
    }
}
