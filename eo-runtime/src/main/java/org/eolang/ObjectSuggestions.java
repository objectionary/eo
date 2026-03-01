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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

/**
 * Suggests similar EO objects when an object is not found.
 * @since 0.52
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.GodClass"})
final class ObjectSuggestions {
    /**
     * File protocol prefix.
     */
    private static final String FILE = "file:";

    /**
     * Available EO objects.
     */
    private final Set<String> objects;

    /**
     * Ctor.
     */
    ObjectSuggestions() {
        this.objects = new HashSet<>(0);
    }

    /**
     * Suggests similar objects for a not found object.
     * @param notfound Object not found
     * @return Suggestion text or empty
     */
    String suggest(final String notfound) {
        this.load();
        final String target = ObjectSuggestions.toEo(notfound);
        final List<Map.Entry<String, Double>> list = new ArrayList<>(0);
        for (final String obj : this.objects) {
            final double sim = ObjectSuggestions.sim(target, obj);
            if (sim > 0) {
                list.add(new AbstractMap.SimpleEntry<>(obj, sim));
            }
        }
        list.sort(
            Comparator.comparingDouble(
                (Map.Entry<String, Double> ent) -> ent.getValue()
            ).reversed()
        );
        final StringBuilder out = new StringBuilder(64);
        if (!list.isEmpty()) {
            out.append("\n\nDid you mean?");
            final int max = Math.min(5, list.size());
            for (int idx = 0; idx < max; ++idx) {
                out.append("\n  - ").append(list.get(idx).getKey());
            }
        }
        return out.toString();
    }

    /**
     * Loads objects if not loaded.
     */
    @SuppressWarnings({"PMD.AvoidCatchingGenericException", "PMD.EmptyCatchBlock"})
    private void load() {
        if (!this.objects.isEmpty()) {
            return;
        }
        try {
            final Enumeration<URL> res = Thread.currentThread()
                .getContextClassLoader().getResources("EOorg");
            while (res.hasMoreElements()) {
                final URL url = res.nextElement();
                if ("file".equals(url.getProtocol())) {
                    this.scanDir(new File(url.toURI()), "EOorg");
                } else if ("jar".equals(url.getProtocol())) {
                    this.scanJar(url);
                }
            }
        } catch (final IOException | URISyntaxException ignored) {
        }
    }

    /**
     * Scans directory.
     * @param dir Directory
     * @param pkg Package
     */
    private void scanDir(final File dir, final String pkg) {
        final File[] files = dir.listFiles();
        if (files == null) {
            return;
        }
        for (final File file : files) {
            if (file.isDirectory()) {
                this.scanDir(file, String.format("%s.%s", pkg, file.getName()));
            } else if (file.getName().endsWith(".class")) {
                this.add(pkg, file.getName());
            }
        }
    }

    /**
     * Scans JAR.
     * @param url JAR URL
     * @throws IOException If fails
     */
    private void scanJar(final URL url) throws IOException {
        final String path = url.getPath();
        final int sep = path.indexOf('!');
        if (sep <= 0 || !path.startsWith(ObjectSuggestions.FILE)) {
            return;
        }
        try (JarFile jar = new JarFile(path.substring(5, sep))) {
            final Enumeration<JarEntry> ents = jar.entries();
            while (ents.hasMoreElements()) {
                final String name = ents.nextElement().getName();
                if (name.startsWith("EOorg/") && name.endsWith(".class")) {
                    final String cls = name.substring(0, name.length() - 6)
                        .replace('/', '.');
                    final int dot = cls.lastIndexOf('.');
                    if (dot > 0) {
                        this.add(
                            cls.substring(0, dot),
                            String.format("%s.class", cls.substring(dot + 1))
                        );
                    }
                }
            }
        }
    }

    /**
     * Adds class.
     * @param pkg Package
     * @param file File name
     */
    private void add(final String pkg, final String file) {
        if (file.contains("package-info")) {
            return;
        }
        final String cls = file.substring(0, file.length() - 6);
        final String eon = ObjectSuggestions.toEo(String.format("%s.%s", pkg, cls));
        if (!eon.isEmpty()) {
            this.objects.add(eon);
        }
    }

    /**
     * Converts Java name to EO.
     * @param java Java name
     * @return EO name
     */
    private static String toEo(final String java) {
        final String[] parts = java.split("\\.");
        final StringBuilder out = new StringBuilder(64);
        boolean valid = true;
        for (int pidx = 0; pidx < parts.length && valid; ++pidx) {
            final String part = parts[pidx];
            if (part.startsWith("EO")) {
                valid = ObjectSuggestions.convertPart(out, part);
            } else {
                valid = false;
            }
        }
        final String result;
        if (valid) {
            result = out.toString();
        } else {
            result = "";
        }
        return result;
    }

    /**
     * Converts one part.
     * @param out Output builder
     * @param part Part to convert
     * @return True if valid
     */
    private static boolean convertPart(final StringBuilder out, final String part) {
        final String[] subs = part.split("\\$");
        boolean valid = true;
        for (int sidx = 0; sidx < subs.length && valid; ++sidx) {
            final String sub = subs[sidx];
            if (sub.startsWith("EO")) {
                ObjectSuggestions.appendSub(out, sub, sidx);
            } else {
                valid = false;
            }
        }
        return valid;
    }

    /**
     * Appends converted substring.
     * @param out Output builder
     * @param sub Substring to convert
     * @param sidx Substring index
     */
    private static void appendSub(
        final StringBuilder out, final String sub, final int sidx
    ) {
        if (out.length() > 0) {
            final char sep;
            if (sidx > 0) {
                sep = '$';
            } else {
                sep = '.';
            }
            out.append(sep);
        }
        out.append(sub.substring(2).replace('_', '-'));
    }

    /**
     * Calculates similarity.
     * @param src Source
     * @param tgt Target
     * @return Score
     */
    private static double sim(final String src, final String tgt) {
        final int dist = ObjectSuggestions.dist(src, tgt);
        final int max = Math.max(src.length(), tgt.length());
        final double result;
        if (max == 0) {
            result = 1.0;
        } else {
            result = 1.0 - (double) dist / max;
        }
        return result;
    }

    /**
     * Calculates Levenshtein distance.
     * @param src Source
     * @param tgt Target
     * @return Distance
     */
    private static int dist(final String src, final String tgt) {
        final int slen = src.length();
        final int tlen = tgt.length();
        final int result;
        if (slen == 0) {
            result = tlen;
        } else if (tlen == 0) {
            result = slen;
        } else {
            result = ObjectSuggestions.compute(src, tgt, slen, tlen);
        }
        return result;
    }

    /**
     * Computes distance.
     * @param src Source
     * @param tgt Target
     * @param slen Source length
     * @param tlen Target length
     * @return Distance
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    private static int compute(
        final String src, final String tgt, final int slen, final int tlen
    ) {
        int[] prev = new int[tlen + 1];
        int[] curr = new int[tlen + 1];
        for (int idx = 0; idx <= tlen; ++idx) {
            prev[idx] = idx;
        }
        for (int sid = 1; sid <= slen; ++sid) {
            curr[0] = sid;
            for (int tid = 1; tid <= tlen; ++tid) {
                final int cost;
                if (src.charAt(sid - 1) == tgt.charAt(tid - 1)) {
                    cost = 0;
                } else {
                    cost = 1;
                }
                curr[tid] = Math.min(
                    Math.min(curr[tid - 1] + 1, prev[tid] + 1),
                    prev[tid - 1] + cost
                );
            }
            final int[] tmp = prev;
            prev = curr;
            curr = tmp;
        }
        return prev[tlen];
    }
}
