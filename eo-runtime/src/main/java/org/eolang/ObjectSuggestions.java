/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.security.CodeSource;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Suggests similar EO objects when an object is not found.
 *
 * <p>This class scans available EO objects and finds the closest matches
 * using Levenshtein distance, word matching, and partial matching algorithms.
 *
 * @since 0.52
 */
final class ObjectSuggestions {

    /**
     * Pattern for package-info classes.
     */
    private static final Pattern PACKAGE_INFO = Pattern.compile("package-info");

    /**
     * Pattern to extract EO object names from Java class names.
     * Matches class names like "EOorg.EOeolang.EOstdout" and extracts "org.eolang.stdout".
     */
    private static final Pattern EO_CLASS = Pattern.compile(
        "^EO([^$]+)(?:\\$EO(.*))?$"
    );

    /**
     * Maximum number of suggestions to return.
     */
    private static final int MAX_SUGGESTIONS = 5;

    /**
     * Cached set of available EO object names.
     */
    private final Set<String> available;

    /**
     * Ctor.
     */
    ObjectSuggestions() {
        this.available = new HashSet<>(0);
    }

    /**
     * Gets suggestions for a not found object.
     * @param notfound The object that was not found (in Java notation, e.g., "EOorg.EOeolang.EOio.EOstd1out")
     * @return Formatted suggestion string, or empty string if no suggestions
     */
    public String suggest(final String notfound) {
        this.ensureLoaded();
        if (this.available.isEmpty()) {
            return "";
        }
        final String target = ObjectSuggestions.javaToEo(notfound);
        final List<Suggestion> suggestions = new ArrayList<>(0);
        for (final String candidate : this.available) {
            final double score = ObjectSuggestions.similarity(target, candidate);
            if (score > 0) {
                suggestions.add(new Suggestion(candidate, score));
            }
        }
        if (suggestions.isEmpty()) {
            return "";
        }
        Collections.sort(suggestions, Comparator.comparingDouble(Suggestion::score).reversed());
        final StringBuilder result = new StringBuilder(64);
        result.append("\n\nDid you mean?");
        final int limit = Math.min(ObjectSuggestions.MAX_SUGGESTIONS, suggestions.size());
        for (int idx = 0; idx < limit; ++idx) {
            result.append("\n  - ").append(suggestions.get(idx).name());
        }
        return result.toString();
    }

    /**
     * Ensures available objects are loaded.
     */
    private void ensureLoaded() {
        if (!this.available.isEmpty()) {
            return;
        }
        this.loadFromClasspath();
    }

    /**
     * Loads available EO objects from classpath.
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    private void loadFromClasspath() {
        try {
            final ClassLoader loader = Thread.currentThread().getContextClassLoader();
            final Enumeration<URL> resources = loader.getResources("EOorg");
            while (resources.hasMoreElements()) {
                final URL resource = resources.nextElement();
                final String protocol = resource.getProtocol();
                if ("file".equals(protocol)) {
                    this.scanDirectory(new File(resource.toURI()), "EOorg");
                } else if ("jar".equals(protocol)) {
                    this.scanJar(resource);
                }
            }
        } catch (final Exception ex) {
            // Silently ignore if we can't load classes - suggestions are optional
        }
    }

    /**
     * Scans a directory for EO classes.
     * @param directory Directory to scan
     * @param packagename Current package name
     */
    private void scanDirectory(final File directory, final String packagename) {
        if (!directory.exists()) {
            return;
        }
        final File[] files = directory.listFiles();
        if (files == null) {
            return;
        }
        for (final File file : files) {
            if (file.isDirectory()) {
                this.scanDirectory(
                    file,
                    String.format("%s.%s", packagename, file.getName())
                );
            } else if (file.getName().endsWith(".class")) {
                this.processClass(packagename, file.getName());
            }
        }
    }

    /**
     * Scans a JAR file for EO classes.
     * @param resource JAR URL
     * @throws IOException If JAR cannot be read
     */
    private void scanJar(final URL resource) throws IOException {
        final String jarpath = resource.getPath();
        final int separator = jarpath.indexOf('!');
        if (separator < 0) {
            return;
        }
        final String path = jarpath.substring(5, separator);
        try (JarFile jar = new JarFile(path)) {
            final Enumeration<JarEntry> entries = jar.entries();
            while (entries.hasMoreElements()) {
                final JarEntry entry = entries.nextElement();
                final String name = entry.getName();
                if (name.startsWith("EOorg/") && name.endsWith(".class")) {
                    final String classname = name
                        .substring(0, name.length() - 6)
                        .replace('/', '.');
                    final int lastdot = classname.lastIndexOf('.');
                    if (lastdot > 0) {
                        this.processClass(
                            classname.substring(0, lastdot),
                            classname.substring(lastdot + 1) + ".class"
                        );
                    }
                }
            }
        }
    }

    /**
     * Processes a class file and extracts EO object name.
     * @param packagename Package name
     * @param filename Class file name
     */
    private void processClass(final String packagename, final String filename) {
        if (ObjectSuggestions.PACKAGE_INFO.matcher(filename).find()) {
            return;
        }
        final String classname = filename.substring(0, filename.length() - 6);
        final String fullname = String.format("%s.%s", packagename, classname);
        final String eoname = ObjectSuggestions.javaToEo(fullname);
        if (!eoname.isEmpty()) {
            this.available.add(eoname);
        }
    }

    /**
     * Converts Java class name to EO object name.
     * @param java Java class name (e.g., "EOorg.EOeolang.EOstdout")
     * @return EO object name (e.g., "org.eolang.stdout"), or empty if not EO class
     */
    private static String javaToEo(final String java) {
        final String[] parts = java.split("\\.");
        final StringBuilder result = new StringBuilder(64);
        for (final String part : parts) {
            final String converted = ObjectSuggestions.convertPart(part);
            if (converted.isEmpty()) {
                return "";
            }
            if (result.length() > 0) {
                result.append('.');
            }
            result.append(converted);
        }
        return result.toString();
    }

    /**
     * Converts a single part of Java class name to EO format.
     * @param part Part to convert (e.g., "EOstdout" or "EOstdout$EOwrite")
     * @return Converted part, or empty if not a valid EO part
     */
    private static String convertPart(final String part) {
        if (!part.startsWith("EO")) {
            return "";
        }
        final StringBuilder result = new StringBuilder(32);
        final String[] subparts = part.split("\\$");
        for (final String subpart : subparts) {
            if (!subpart.startsWith("EO")) {
                return "";
            }
            if (result.length() > 0) {
                result.append('$');
            }
            result.append(
                subpart.substring(2).replace('_', '-')
            );
        }
        return result.toString();
    }

    /**
     * Calculates similarity score between two strings.
     * Combines Levenshtein distance, word matching, and partial matching.
     * @param target Target string (what user typed)
     * @param candidate Candidate string (possible suggestion)
     * @return Similarity score (higher is better)
     */
    private static double similarity(final String target, final String candidate) {
        final double levenshtein = ObjectSuggestions.levenshteinSimilarity(target, candidate);
        final double wordmatch = ObjectSuggestions.wordMatchScore(target, candidate);
        final double partial = ObjectSuggestions.partialMatchScore(target, candidate);
        return levenshtein * 0.4 + wordmatch * 0.3 + partial * 0.3;
    }

    /**
     * Calculates Levenshtein-based similarity.
     * @param first First string
     * @param second Second string
     * @return Similarity score between 0 and 1
     */
    private static double levenshteinSimilarity(final String first, final String second) {
        final int distance = ObjectSuggestions.levenshteinDistance(first, second);
        final int maxlen = Math.max(first.length(), second.length());
        if (maxlen == 0) {
            return 1.0;
        }
        return 1.0 - (double) distance / maxlen;
    }

    /**
     * Calculates Levenshtein distance between two strings.
     * @param first First string
     * @param second Second string
     * @return Edit distance
     */
    private static int levenshteinDistance(final String first, final String second) {
        final int flen = first.length();
        final int slen = second.length();
        if (flen == 0) {
            return slen;
        }
        if (slen == 0) {
            return flen;
        }
        int[] prev = new int[slen + 1];
        int[] curr = new int[slen + 1];
        for (int idx = 0; idx <= slen; ++idx) {
            prev[idx] = idx;
        }
        for (int fid = 1; fid <= flen; ++fid) {
            curr[0] = fid;
            for (int sid = 1; sid <= slen; ++sid) {
                final int cost;
                if (first.charAt(fid - 1) == second.charAt(sid - 1)) {
                    cost = 0;
                } else {
                    cost = 1;
                }
                curr[sid] = Math.min(
                    Math.min(curr[sid - 1] + 1, prev[sid] + 1),
                    prev[sid - 1] + cost
                );
            }
            final int[] temp = prev;
            prev = curr;
            curr = temp;
        }
        return prev[slen];
    }

    /**
     * Calculates word match score.
     * @param target Target string
     * @param candidate Candidate string
     * @return Score between 0 and 1
     */
    private static double wordMatchScore(final String target, final String candidate) {
        final String[] twords = target.split("[.$-]");
        final String[] cwords = candidate.split("[.$-]");
        int matches = 0;
        for (final String tword : twords) {
            for (final String cword : cwords) {
                if (tword.equalsIgnoreCase(cword)) {
                    matches += 1;
                    break;
                }
            }
        }
        if (twords.length == 0) {
            return 0.0;
        }
        return (double) matches / twords.length;
    }

    /**
     * Calculates partial match score (substring matching).
     * @param target Target string
     * @param candidate Candidate string
     * @return Score between 0 and 1
     */
    private static double partialMatchScore(final String target, final String candidate) {
        final String tlower = target.toLowerCase();
        final String clower = candidate.toLowerCase();
        if (clower.contains(tlower) || tlower.contains(clower)) {
            return 1.0;
        }
        final String[] tparts = target.split("[.$-]");
        int partials = 0;
        for (final String tpart : tparts) {
            if (tpart.length() >= 3 && clower.contains(tpart.toLowerCase())) {
                partials += 1;
            }
        }
        if (tparts.length == 0) {
            return 0.0;
        }
        return (double) partials / tparts.length;
    }

    /**
     * Suggestion holder.
     *
     * @since 0.52
     */
    private static final class Suggestion {
        /**
         * Object name.
         */
        private final String obj;

        /**
         * Similarity score.
         */
        private final double sim;

        /**
         * Ctor.
         * @param name Object name
         * @param score Similarity score
         */
        Suggestion(final String name, final double score) {
            this.obj = name;
            this.sim = score;
        }

        /**
         * Gets the name.
         * @return Object name
         */
        String name() {
            return this.obj;
        }

        /**
         * Gets the score.
         * @return Similarity score
         */
        double score() {
            return this.sim;
        }
    }
}
