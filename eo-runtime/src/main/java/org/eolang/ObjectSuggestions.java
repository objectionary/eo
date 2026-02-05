/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Pattern;

/**
 * Suggests similar EO objects when an object is not found.
 *
 * <p>This class scans available EO objects and finds the closest matches
 * using Levenshtein distance algorithm.
 *
 * @since 0.52
 */
@SuppressWarnings("PMD.TooManyMethods")
final class ObjectSuggestions {

    /**
     * Pattern for package-info classes.
     */
    private static final Pattern PACKAGE_INFO = Pattern.compile("package-info");

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
     * @param notfound The object that was not found
     * @return Formatted suggestion string, or empty string if no suggestions
     */
    String suggest(final String notfound) {
        this.ensureLoaded();
        final String result;
        if (this.available.isEmpty()) {
            result = "";
        } else {
            result = this.buildSuggestions(notfound);
        }
        return result;
    }

    /**
     * Builds formatted suggestions string.
     * @param notfound The object that was not found
     * @return Formatted suggestions or empty string
     */
    private String buildSuggestions(final String notfound) {
        final String target = ObjectSuggestions.javaToEo(notfound);
        final List<Suggestion> suggestions = this.collectSuggestions(target);
        final String result;
        if (suggestions.isEmpty()) {
            result = "";
        } else {
            result = this.formatSuggestions(suggestions);
        }
        return result;
    }

    /**
     * Collects and sorts suggestions.
     * @param target Target EO object name
     * @return Sorted list of suggestions
     */
    private List<Suggestion> collectSuggestions(final String target) {
        final List<Suggestion> suggestions = new ArrayList<>(0);
        for (final String candidate : this.available) {
            final double score = ObjectSuggestions.similarity(target, candidate);
            if (score > 0) {
                suggestions.add(new Suggestion(candidate, score));
            }
        }
        Collections.sort(
            suggestions,
            Comparator.comparingDouble(Suggestion::score).reversed()
        );
        return suggestions;
    }

    /**
     * Formats suggestions into output string.
     * @param suggestions List of suggestions
     * @return Formatted string
     */
    private String formatSuggestions(final List<Suggestion> suggestions) {
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
        if (this.available.isEmpty()) {
            this.loadFromClasspath();
        }
    }

    /**
     * Loads available EO objects from classpath.
     */
    @SuppressWarnings({"PMD.AvoidCatchingGenericException", "PMD.EmptyCatchBlock"})
    private void loadFromClasspath() {
        try {
            final ClassLoader loader = Thread.currentThread().getContextClassLoader();
            final Enumeration<URL> resources = loader.getResources("EOorg");
            while (resources.hasMoreElements()) {
                this.processResource(resources.nextElement());
            }
        } catch (final IOException ignored) {
        }
    }

    /**
     * Processes a resource URL.
     * @param resource Resource URL
     * @throws IOException If reading fails
     */
    private void processResource(final URL resource) throws IOException {
        final String protocol = resource.getProtocol();
        if ("file".equals(protocol)) {
            this.scanFileResource(resource);
        } else if ("jar".equals(protocol)) {
            this.scanJar(resource);
        }
    }

    /**
     * Scans a file resource.
     * @param resource File resource URL
     */
    @SuppressWarnings("PMD.AvoidCatchingGenericException")
    private void scanFileResource(final URL resource) {
        try {
            this.scanDirectory(new File(resource.toURI()), "EOorg");
        } catch (final Exception ignored) {
        }
    }

    /**
     * Scans a directory for EO classes.
     * @param directory Directory to scan
     * @param packagename Current package name
     */
    private void scanDirectory(final File directory, final String packagename) {
        final File[] files = directory.listFiles();
        if (files != null) {
            for (final File file : files) {
                this.processFile(file, packagename);
            }
        }
    }

    /**
     * Processes a file or directory.
     * @param file File to process
     * @param packagename Current package name
     */
    private void processFile(final File file, final String packagename) {
        if (file.isDirectory()) {
            this.scanDirectory(
                file,
                String.format("%s.%s", packagename, file.getName())
            );
        } else if (file.getName().endsWith(".class")) {
            this.processClass(packagename, file.getName());
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
        if (separator >= 0) {
            final String path = jarpath.substring(5, separator);
            try (JarFile jar = new JarFile(path)) {
                this.processJarEntries(jar);
            }
        }
    }

    /**
     * Processes JAR entries.
     * @param jar JAR file
     */
    private void processJarEntries(final JarFile jar) {
        final Enumeration<JarEntry> entries = jar.entries();
        while (entries.hasMoreElements()) {
            this.processJarEntry(entries.nextElement());
        }
    }

    /**
     * Processes a single JAR entry.
     * @param entry JAR entry
     */
    private void processJarEntry(final JarEntry entry) {
        final String name = entry.getName();
        if (name.startsWith("EOorg/") && name.endsWith(".class")) {
            final String classname = name.substring(0, name.length() - 6).replace('/', '.');
            final int lastdot = classname.lastIndexOf('.');
            if (lastdot > 0) {
                this.processClass(
                    classname.substring(0, lastdot),
                    classname.substring(lastdot + 1).concat(".class")
                );
            }
        }
    }

    /**
     * Processes a class file and extracts EO object name.
     * @param packagename Package name
     * @param filename Class file name
     */
    private void processClass(final String packagename, final String filename) {
        if (!ObjectSuggestions.PACKAGE_INFO.matcher(filename).find()) {
            final String classname = filename.substring(0, filename.length() - 6);
            final String fullname = String.format("%s.%s", packagename, classname);
            final String eoname = ObjectSuggestions.javaToEo(fullname);
            if (!eoname.isEmpty()) {
                this.available.add(eoname);
            }
        }
    }

    /**
     * Converts Java class name to EO object name.
     * @param java Java class name
     * @return EO object name or empty string
     */
    private static String javaToEo(final String java) {
        final String[] parts = java.split("\\.");
        final StringBuilder result = new StringBuilder(64);
        boolean valid = true;
        for (final String part : parts) {
            final String converted = ObjectSuggestions.convertPart(part);
            if (converted.isEmpty()) {
                valid = false;
                break;
            }
            if (result.length() > 0) {
                result.append('.');
            }
            result.append(converted);
        }
        final String output;
        if (valid) {
            output = result.toString();
        } else {
            output = "";
        }
        return output;
    }

    /**
     * Converts a single part of Java class name to EO format.
     * @param part Part to convert
     * @return Converted part or empty string
     */
    private static String convertPart(final String part) {
        final StringBuilder result = new StringBuilder(32);
        boolean valid = part.startsWith("EO");
        if (valid) {
            final String[] subparts = part.split("\\$");
            for (final String subpart : subparts) {
                if (!subpart.startsWith("EO")) {
                    valid = false;
                    break;
                }
                if (result.length() > 0) {
                    result.append('$');
                }
                result.append(subpart.substring(2).replace('_', '-'));
            }
        }
        final String output;
        if (valid) {
            output = result.toString();
        } else {
            output = "";
        }
        return output;
    }

    /**
     * Calculates similarity score using Levenshtein distance.
     * @param target Target string
     * @param candidate Candidate string
     * @return Similarity score between 0 and 1
     */
    private static double similarity(final String target, final String candidate) {
        final int distance = ObjectSuggestions.levenshteinDistance(target, candidate);
        final int maxlen = Math.max(target.length(), candidate.length());
        final double score;
        if (maxlen == 0) {
            score = 1.0;
        } else {
            score = 1.0 - (double) distance / maxlen;
        }
        return score;
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
        final int result;
        if (flen == 0) {
            result = slen;
        } else if (slen == 0) {
            result = flen;
        } else {
            result = ObjectSuggestions.computeDistance(first, second, flen, slen);
        }
        return result;
    }

    /**
     * Computes Levenshtein distance using dynamic programming.
     * @param first First string
     * @param second Second string
     * @param flen First string length
     * @param slen Second string length
     * @return Edit distance
     * @checkstyle ParameterNumberCheck (5 lines)
     */
    private static int computeDistance(
        final String first, final String second, final int flen, final int slen
    ) {
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
