/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Suggests the closest EO object names when an object is not found, so the
 * "Couldn't find object ..." error points the user at what was meant (#4520).
 * The candidates are the EO objects available on the classpath, collected
 * when this object is built.
 * @since 0.74.0
 */
@SuppressWarnings({"PMD.GodClass", "PMD.EmptyCatchBlock"})
final class ObjectSuggestions {

    /**
     * The root package that hosts the transpiled EO objects.
     */
    private static final String ROOT = "org/eolang/";

    /**
     * The EO object names found on the classpath.
     */
    private final List<String> names;

    /**
     * Ctor.
     * @param loader The class loader to scan for EO objects
     */
    ObjectSuggestions(final ClassLoader loader) {
        this.names = ObjectSuggestions.scan(loader);
    }

    /**
     * Ctor.
     */
    ObjectSuggestions() {
        this(Thread.currentThread().getContextClassLoader());
    }

    /**
     * The five closest EO object names for a missing one.
     * @param fqn FQN of the missing object, e.g. {@code Φ.org.eolang.io.std1out}
     * @return A "Did you mean?" block, or an empty string
     */
    String suggest(final String fqn) {
        String target = fqn;
        if (target.startsWith("Φ.")) {
            target = target.substring(2);
        }
        if (target.startsWith("org.eolang.")) {
            target = target.substring("org.eolang.".length());
        }
        final List<Map.Entry<String, Integer>> ranked = new ArrayList<>(0);
        for (final String name : this.names) {
            ranked.add(
                new AbstractMap.SimpleImmutableEntry<>(
                    name, ObjectSuggestions.dist(target, name)
                )
            );
        }
        ranked.sort(Comparator.comparingInt(Map.Entry::getValue));
        final StringBuilder out = new StringBuilder(64);
        for (int idx = 0; idx < Math.min(5, ranked.size()); ++idx) {
            out.append(String.format("%n  - %s", ranked.get(idx).getKey()));
        }
        final String result;
        if (out.length() > 0) {
            result = String.format("%n%nDid you mean?%s", out);
        } else {
            result = "";
        }
        return result;
    }

    /**
     * Collect every EO object name on the classpath by scanning the
     * {@link #ROOT} package — a directory when running from classes, or an
     * entry inside a jar.
     * @param loader The class loader to scan
     * @return The EO object names found
     */
    private static List<String> scan(final ClassLoader loader) {
        final List<String> found = new ArrayList<>(0);
        try {
            final Enumeration<URL> res = loader
                .getResources(ObjectSuggestions.ROOT);
            while (res.hasMoreElements()) {
                final URL url = res.nextElement();
                if ("file".equals(url.getProtocol())) {
                    ObjectSuggestions.tree(found, new File(url.toURI()));
                } else if ("jar".equals(url.getProtocol())) {
                    ObjectSuggestions.jar(found, url);
                }
            }
        } catch (final IOException | URISyntaxException ex) {
            // the classpath cannot be read; the list stays empty
        }
        return found;
    }

    /**
     * Fill {@code found} with the EO names of every class under a directory.
     * @param found The collected names
     * @param dir The {@link #ROOT} directory
     */
    private static void tree(final List<String> found, final File dir) {
        final File[] children = dir.listFiles();
        if (children != null) {
            for (final File child : children) {
                if (child.isDirectory()) {
                    ObjectSuggestions.tree(found, child);
                } else {
                    ObjectSuggestions.plus(
                        found, child.getPath().substring(dir.getPath().length() + 1)
                    );
                }
            }
        }
    }

    /**
     * Fill {@code found} with the EO names of every class inside a jar.
     * @param found The collected names
     * @param url URL of the jar
     */
    private static void jar(final List<String> found, final URL url) {
        try (JarFile jar = new JarFile(url.getPath().replace("file:", ""))) {
            final Enumeration<JarEntry> entries = jar.entries();
            while (entries.hasMoreElements()) {
                final String path = entries.nextElement().getName();
                if (path.startsWith(ObjectSuggestions.ROOT)
                    && path.endsWith(".class")) {
                    ObjectSuggestions.plus(
                        found,
                        path.replace('/', '.').substring(ObjectSuggestions.ROOT.length())
                    );
                }
            }
        } catch (final IOException ex) {
            // the jar cannot be read; the list stays empty
        }
    }

    /**
     * Add one converted class name to the collected names.
     * @param found The collected names
     * @param path The class path below {@link #ROOT}
     */
    private static void plus(final List<String> found, final String path) {
        final String name = ObjectSuggestions.toEo(path.replace(".class", ""));
        if (!name.isEmpty()) {
            found.add(name);
        }
    }

    /**
     * Invert {@link JavaPath} for one class name: {@code EO_<pkg>.EO<object>}
     * becomes the EO name, with the dash encoding reversed and test classes
     * dropped.
     * @param java The class name, e.g. {@code EO_io.EOstdout}
     * @return The EO name, or an empty string for a non-EO class
     */
    private static String toEo(final String java) {
        final StringBuilder out = new StringBuilder(java.length());
        boolean valid = true;
        for (final String part : java.split("\\.", -1)) {
            final String segment = ObjectSuggestions.segment(part);
            if (segment == null) {
                valid = false;
                break;
            }
            if (out.length() > 0) {
                out.append('.');
            }
            out.append(ObjectSuggestions.dashes(segment));
        }
        String result = "";
        if (valid && !out.toString().endsWith("Test")) {
            result = out.toString();
        }
        return result;
    }

    /**
     * The EO segment of one class-name part, or {@code null} when the part
     * is not a plain EO package or object name.
     * @param part The part, e.g. {@code EO_io} or {@code EOstdout}
     * @return The EO segment, or {@code null}
     */
    private static String segment(final String part) {
        final String result;
        if (part.startsWith("EO_") && ObjectSuggestions.clean(part)) {
            result = part.substring(3);
        } else if (part.startsWith("EO") && ObjectSuggestions.clean(part)) {
            result = part.substring(2);
        } else {
            result = null;
        }
        return result;
    }

    /**
     * Whether a class-name part is not nested, anonymous or package-info.
     * @param part The part
     * @return True when it is a plain EO name
     */
    private static boolean clean(final String part) {
        return !part.isEmpty() && !part.contains("$") && !part.contains("package-info");
    }

    /**
     * Reverse the {@link JavaPath} dash/underscore encoding: a double
     * underscore was an underscore and a single one a dash.
     * @param seg The encoded segment
     * @return The decoded segment
     */
    private static String dashes(final String seg) {
        final StringBuilder out = new StringBuilder(seg.length());
        int idx = 0;
        while (idx < seg.length()) {
            final char glyph = seg.charAt(idx);
            if (glyph == '_' && idx + 1 < seg.length() && seg.charAt(idx + 1) == '_') {
                out.append('_');
                idx = idx + 2;
            } else if (glyph == '_') {
                out.append('-');
                idx = idx + 1;
            } else {
                out.append(glyph);
                idx = idx + 1;
            }
        }
        return out.toString();
    }

    /**
     * The Levenshtein distance between two strings.
     * @param src Source
     * @param tgt Target
     * @return The distance
     */
    private static int dist(final String src, final String tgt) {
        final int[] row = new int[tgt.length() + 1];
        for (int idx = 0; idx <= tgt.length(); ++idx) {
            row[idx] = idx;
        }
        for (int sidx = 1; sidx <= src.length(); ++sidx) {
            int prev = row[0];
            row[0] = sidx;
            for (int tidx = 1; tidx <= tgt.length(); ++tidx) {
                final int cur = row[tidx];
                row[tidx] = Math.min(
                    Math.min(row[tidx] + 1, row[tidx - 1] + 1),
                    prev + ObjectSuggestions.cost(src, tgt, sidx, tidx)
                );
                prev = cur;
            }
        }
        return row[tgt.length()];
    }

    /**
     * The substitution cost of one cell.
     * @param src Source
     * @param tgt Target
     * @param sidx Source index
     * @param tidx Target index
     * @return Zero when the characters match, one otherwise
     */
    private static int cost(final String src, final String tgt,
        final int sidx, final int tidx) {
        final int result;
        if (src.charAt(sidx - 1) == tgt.charAt(tidx - 1)) {
            result = 0;
        } else {
            result = 1;
        }
        return result;
    }
}
