/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */

package org.eolang;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Locale;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Suggestions for EO objects.
 * @since 1.0
 * @checkstyle ClassDataAbstractionCouplingCheck (500 lines)
 */
@SuppressWarnings({"PMD.TooManyMethods", "PMD.GodClass"})
final class PhSuggestions {

    /**
     * Default number of suggestions.
     */
    private static final int LIMIT = 5;

    /**
     * Class file extension.
     */
    private static final String EXT = ".class";

    /**
     * Java root package.
     */
    private static final String ROOT = "org.eolang.";

    /**
     * EO global object.
     */
    private static final String GLOBAL = "Φ";

    /**
     * Resource root package.
     */
    private static final String RROOT = "org/eolang/";

    /**
     * Java object prefix.
     */
    private static final String EOP = "EO";

    /**
     * Separators to remove for compact names.
     */
    private static final Pattern COMPACT = Pattern.compile("[_\\s./-]+");

    /**
     * Separators in EO tokens.
     */
    private static final Pattern TOKENS = Pattern.compile("(?=[A-Z])|[^A-Za-z]+");

    /**
     * Minimum similarity score.
     */
    private static final double MIN_SCORE = 0.34D;

    /**
     * Jaro-Winkler score weight.
     */
    private static final double JWGT = 0.5D;

    /**
     * Jaccard score weight.
     */
    private static final double JGT = 0.3D;

    /**
     * Containment score weight.
     */
    private static final double CGT = 0.1D;

    /**
     * Prefix score weight.
     */
    private static final double PGT = 0.1D;

    /**
     * Maximum common prefix length.
     */
    private static final int PREF = 4;

    /**
     * Jaro-Winkler prefix scale.
     */
    private static final double PSCALE = 0.1D;

    /**
     * Maximum length penalty.
     */
    private static final double MAX_PENALTY = 0.15D;

    /**
     * Length penalty per character.
     */
    private static final double PENALTY = 0.01D;

    /**
     * Maximum similarity score.
     */
    private static final double TOP = 1.0D;

    /**
     * Empty similarity score.
     */
    private static final double ZERO = 0.0D;

    /**
     * Candidates.
     */
    private final Collection<String> candidates;

    /**
     * Ctor.
     * @checkstyle ConstructorsCodeFreeCheck (3 lines)
     */
    PhSuggestions() {
        this(PhSuggestions.discover());
    }

    /**
     * Ctor.
     * @param candidates Candidates
     */
    PhSuggestions(final Collection<String> candidates) {
        this.candidates = candidates;
    }

    /**
     * Make a message with suggestions.
     * @param name Requested EO object name
     * @return Message suffix
     */
    String message(final String name) {
        return this.message(name, PhSuggestions.LIMIT);
    }

    /**
     * Make a message with suggestions.
     * @param name Requested EO object name
     * @param limit Max suggestions
     * @return Message suffix
     */
    String message(final String name, final int limit) {
        final String suffix;
        final List<String> found = this.suggestions(name, limit);
        if (found.isEmpty()) {
            suffix = "";
        } else {
            suffix = String.format(
                "%n%nDid you mean?%n%s",
                found.stream()
                    .map(item -> String.format("  - %s", item))
                    .collect(Collectors.joining(System.lineSeparator()))
            );
        }
        return suffix;
    }

    /**
     * Suggest objects.
     * @param name Requested EO object name
     * @param limit Max suggestions
     * @return Suggestions
     */
    List<String> suggestions(final String name, final int limit) {
        final String normalized = PhSuggestions.normalize(name);
        return this.candidates.stream()
            .map(PhSuggestions::normalize)
            .filter(Predicate.not(String::isEmpty))
            .distinct()
            .filter(candidate -> !candidate.equals(normalized))
            .map(candidate -> PhSuggestions.Ranked.ranked(normalized, candidate))
            .filter(ranked -> ranked.score >= PhSuggestions.MIN_SCORE)
            .sorted()
            .limit(limit)
            .map(ranked -> PhSuggestions.display(name, ranked.name))
            .collect(Collectors.toList());
    }

    /**
     * Object names by class resource path.
     * @param resource Resource path
     * @return EO object names
     */
    static Collection<String> names(final String resource) {
        final Collection<String> names;
        if (resource.startsWith(PhSuggestions.RROOT) && resource.endsWith(PhSuggestions.EXT)) {
            names = PhSuggestions.relative(
                resource.substring(
                    PhSuggestions.RROOT.length(),
                    resource.length() - PhSuggestions.EXT.length()
                )
            );
        } else {
            names = Collections.emptyList();
        }
        return names;
    }

    /**
     * Object names by relative class resource.
     * @param relative Relative class resource
     * @return EO object names
     */
    private static Collection<String> relative(final String relative) {
        final Collection<String> names;
        if (relative.endsWith("/package-info")) {
            names = PhSuggestions.packageInfo(relative);
        } else if (PhSuggestions.internal(relative)) {
            names = Collections.emptyList();
        } else {
            names = PhSuggestions.object(relative);
        }
        return names;
    }

    /**
     * Object names by package-info resource.
     * @param relative Relative class resource
     * @return EO object names
     */
    private static Collection<String> packageInfo(final String relative) {
        return PhSuggestions.singleton(
            PhSuggestions.toObject(
                relative.substring(0, relative.length() - "/package-info".length())
            )
        );
    }

    /**
     * Object names by plain class resource.
     * @param relative Relative class resource
     * @return EO object names
     */
    private static Collection<String> object(final String relative) {
        final String object = PhSuggestions.annotated(relative);
        final Collection<String> names;
        if (object.isEmpty()) {
            names = PhSuggestions.fallback(relative);
        } else {
            names = Collections.singleton(object);
        }
        return names;
    }

    /**
     * Singleton object name.
     * @param object Object name
     * @return Collection
     */
    private static Collection<String> singleton(final String object) {
        final Collection<String> names;
        if (object.isEmpty()) {
            names = Collections.emptyList();
        } else {
            names = Collections.singleton(object);
        }
        return names;
    }

    /**
     * Check whether class resource points to an internal generated object.
     * @param relative Relative class resource
     * @return TRUE if object is internal
     */
    private static boolean internal(final String relative) {
        return Arrays.stream(relative.replace('$', '/').split("/"))
            .anyMatch(part -> part.startsWith("EOΦ") || part.startsWith("EOφ"));
    }

    /**
     * Add object by class resource path.
     * @param resource Resource path
     * @param objects Objects
     */
    private static void add(final String resource, final Collection<String> objects) {
        objects.addAll(PhSuggestions.names(resource));
    }

    /**
     * Read annotated object name.
     * @param relative Relative class resource
     * @return EO object name or empty string
     */
    private static String annotated(final String relative) {
        String object = "";
        try {
            final XmirObject xmir = Class.forName(
                String.format(
                    "%s%s",
                    PhSuggestions.ROOT,
                    relative.replace('/', '.')
                ),
                false,
                Thread.currentThread().getContextClassLoader()
            ).getAnnotation(XmirObject.class);
            if (xmir != null) {
                object = PhSuggestions.inPackage(
                    PhSuggestions.parent(relative),
                    PhSuggestions.original(xmir)
                );
            }
        } catch (final ClassNotFoundException | LinkageError | SecurityException ignored) {
            object = "";
        }
        return object;
    }

    /**
     * Original EO name from annotation.
     * @param xmir Annotation
     * @return EO name
     */
    private static String original(final XmirObject xmir) {
        final String name;
        if (xmir.oname().isEmpty()) {
            name = xmir.name();
        } else {
            name = xmir.oname();
        }
        return name;
    }

    /**
     * Add package to object name.
     * @param parent Parent Java package resource
     * @param object Object name
     * @return Full object name
     */
    private static String inPackage(final String parent, final String object) {
        final String pkg = PhSuggestions.toObject(parent);
        final String full;
        if (pkg.isEmpty() || object.isEmpty() || object.startsWith(String.format("%s.", pkg))) {
            full = object;
        } else {
            full = String.format("%s.%s", pkg, object);
        }
        return full;
    }

    /**
     * Parent resource.
     * @param relative Relative class resource
     * @return Parent resource
     */
    private static String parent(final String relative) {
        final int slash = relative.lastIndexOf('/');
        final String parent;
        if (slash < 0) {
            parent = "";
        } else {
            parent = relative.substring(0, slash);
        }
        return parent;
    }

    /**
     * Fallback conversion from Java path to EO object.
     * @param relative Relative class resource
     * @return Object names
     */
    private static Collection<String> fallback(final String relative) {
        final Collection<String> names;
        final String object = PhSuggestions.toObject(relative.replace('$', '/'));
        if (object.isEmpty() || object.endsWith("Test")) {
            names = Collections.emptyList();
        } else {
            names = Collections.singleton(object);
        }
        return names;
    }

    /**
     * Convert Java path to EO object name.
     * @param path Java resource path
     * @return EO object name
     */
    private static String toObject(final String path) {
        final String object;
        if (path.isEmpty()) {
            object = "";
        } else {
            final List<String> parts = Arrays.asList(path.split("/"));
            if (parts.stream().allMatch(part -> part.startsWith(PhSuggestions.EOP))) {
                object = parts.stream()
                    .map(part -> part.substring(PhSuggestions.EOP.length()))
                    .map(PhSuggestions::dashes)
                    .map(part -> part.replace('φ', '@'))
                    .collect(Collectors.joining("."));
            } else {
                object = "";
            }
        }
        return object;
    }

    /**
     * Convert transpiled Java underscores back to EO dashes.
     * @param name Java name part
     * @return EO name part
     */
    private static String dashes(final String name) {
        final StringBuilder out = new StringBuilder(name.length());
        int pos = 0;
        while (pos < name.length()) {
            out.append(PhSuggestions.dash(name, pos));
            pos += PhSuggestions.skip(name, pos);
        }
        return out.toString();
    }

    /**
     * Convert one Java name character to EO name character.
     * @param name Java name part
     * @param pos Character position
     * @return EO name character
     */
    private static char dash(final String name, final int pos) {
        final char dash;
        if (PhSuggestions.escaped(name, pos)) {
            dash = '_';
        } else if (name.charAt(pos) == '_') {
            dash = '-';
        } else {
            dash = name.charAt(pos);
        }
        return dash;
    }

    /**
     * Number of Java name characters consumed by EO name character.
     * @param name Java name part
     * @param pos Character position
     * @return Characters consumed
     */
    private static int skip(final String name, final int pos) {
        final int skip;
        if (PhSuggestions.escaped(name, pos)) {
            skip = 2;
        } else {
            skip = 1;
        }
        return skip;
    }

    /**
     * Check if underscore is escaped.
     * @param name Java name part
     * @param pos Character position
     * @return TRUE if underscore is escaped
     */
    private static boolean escaped(final String name, final int pos) {
        return name.charAt(pos) == '_' && pos + 1 < name.length()
            && name.charAt(pos + 1) == '_';
    }

    /**
     * Discover objects from classpath.
     * @return EO object names
     */
    private static Collection<String> discover() {
        final Set<String> objects = new TreeSet<>();
        for (final String item : PhSuggestions.classpath()) {
            try {
                final Path path = Paths.get(item);
                if (Files.isDirectory(path)) {
                    PhSuggestions.scan(path, objects);
                } else if (Files.isRegularFile(path) && item.endsWith(".jar")) {
                    PhSuggestions.scan(path.toFile(), objects);
                }
            } catch (final InvalidPathException | SecurityException ignored) {
                continue;
            }
        }
        return objects;
    }

    /**
     * Classpath entries.
     * @return Entries
     */
    private static Iterable<String> classpath() {
        return Arrays.stream(
            System.getProperty("java.class.path", "")
                .split(Pattern.quote(File.pathSeparator))
        ).filter(Predicate.not(String::isEmpty)).collect(Collectors.toList());
    }

    /**
     * Scan classpath directory.
     * @param root Classpath root
     * @param objects Objects
     */
    private static void scan(final Path root, final Collection<String> objects) {
        final Path base = root.resolve("org").resolve("eolang");
        if (Files.exists(base)) {
            try (Stream<Path> paths = Files.walk(base)) {
                paths.filter(Files::isRegularFile)
                    .map(path -> root.relativize(path).toString())
                    .map(path -> path.replace(File.separatorChar, '/'))
                    .forEach(resource -> PhSuggestions.add(resource, objects));
            } catch (final IOException | SecurityException ignored) {
                objects.addAll(Collections.emptyList());
            }
        }
    }

    /**
     * Scan JAR file.
     * @param jar JAR file
     * @param objects Objects
     */
    private static void scan(final File jar, final Collection<String> objects) {
        try (JarFile file = new JarFile(jar)) {
            final Enumeration<JarEntry> entries = file.entries();
            while (entries.hasMoreElements()) {
                final JarEntry entry = entries.nextElement();
                if (!entry.isDirectory()) {
                    PhSuggestions.add(entry.getName(), objects);
                }
            }
        } catch (final IOException | SecurityException ignored) {
            objects.addAll(Collections.emptyList());
        }
    }

    /**
     * Normalize EO object name.
     * @param name Name
     * @return Normalized name
     */
    private static String normalize(final String name) {
        String normalized = name;
        if (normalized.startsWith(String.format("%s.", PhSuggestions.GLOBAL))) {
            normalized = normalized.substring(PhSuggestions.GLOBAL.length() + 1);
        }
        if (normalized.startsWith(PhSuggestions.ROOT)) {
            normalized = normalized.substring(PhSuggestions.ROOT.length());
        }
        return normalized.toLowerCase(Locale.ENGLISH);
    }

    /**
     * Display suggestion in the same namespace style.
     * @param origin Origin
     * @param suggestion Suggestion
     * @return Displayed suggestion
     */
    private static String display(final String origin, final String suggestion) {
        final String displayed;
        if (
            origin.startsWith(
                String.format("%s.%s", PhSuggestions.GLOBAL, PhSuggestions.ROOT)
            ) || origin.startsWith(PhSuggestions.ROOT)
        ) {
            displayed = String.format("%s%s", PhSuggestions.ROOT, suggestion);
        } else {
            displayed = suggestion;
        }
        return displayed;
    }

    /**
     * Tokens of EO object name.
     * @param name EO object name
     * @return Tokens
     */
    private static Collection<String> tokens(final String name) {
        return Arrays.stream(PhSuggestions.TOKENS.split(name))
            .map(part -> part.toLowerCase(Locale.ENGLISH))
            .filter(Predicate.not(String::isEmpty))
            .collect(Collectors.toList());
    }

    /**
     * Compact EO object name.
     * @param name EO object name
     * @return Compact name
     */
    private static String compact(final String name) {
        return PhSuggestions.COMPACT.matcher(
            name.toLowerCase(Locale.ENGLISH)
        ).replaceAll("");
    }

    /**
     * Composite similarity score.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double score(final String left, final String right) {
        return PhSuggestions.clamp(
            PhSuggestions.JWGT * PhSuggestions.jaroWinkler(left, right)
                + PhSuggestions.JGT * PhSuggestions.jaccard(left, right)
                + PhSuggestions.CGT * PhSuggestions.containment(left, right)
                + PhSuggestions.PGT * PhSuggestions.prefixScore(left, right)
                - PhSuggestions.penalty(left, right)
        );
    }

    /**
     * Jaccard similarity.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double jaccard(final String left, final String right) {
        final Set<String> lft = new TreeSet<>(PhSuggestions.tokens(left));
        final Set<String> rgt = new TreeSet<>(PhSuggestions.tokens(right));
        final double similarity;
        if (lft.isEmpty() || rgt.isEmpty()) {
            similarity = PhSuggestions.ZERO;
        } else {
            final Set<String> intersection = new TreeSet<>(lft);
            intersection.retainAll(rgt);
            final Set<String> union = new TreeSet<>(lft);
            union.addAll(rgt);
            similarity = 1.0D * intersection.size() / union.size();
        }
        return similarity;
    }

    /**
     * Containment score.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double containment(final String left, final String right) {
        final String first = PhSuggestions.compact(left);
        final String second = PhSuggestions.compact(right);
        final double score;
        if (!first.isEmpty() && !second.isEmpty()
            && (first.contains(second) || second.contains(first))) {
            score = PhSuggestions.TOP;
        } else {
            score = PhSuggestions.ZERO;
        }
        return score;
    }

    /**
     * Prefix score.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double prefixScore(final String left, final String right) {
        return 1.0D * Math.min(
            PhSuggestions.PREF,
            PhSuggestions.commonPrefix(
                PhSuggestions.compact(left),
                PhSuggestions.compact(right)
            )
        ) / PhSuggestions.PREF;
    }

    /**
     * Length penalty.
     * @param left Left text
     * @param right Right text
     * @return Penalty
     */
    private static double penalty(final String left, final String right) {
        return Math.min(
            PhSuggestions.MAX_PENALTY,
            Math.max(0, right.length() - left.length()) * PhSuggestions.PENALTY
        );
    }

    /**
     * Jaro-Winkler similarity.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double jaroWinkler(final String left, final String right) {
        final double jaro = PhSuggestions.jaro(left, right);
        return PhSuggestions.clamp(
            jaro + 1.0D * PhSuggestions.bprefix(left, right) * PhSuggestions.PSCALE
                * (PhSuggestions.TOP - jaro)
        );
    }

    /**
     * Bounded common prefix.
     * @param left Left text
     * @param right Right text
     * @return Prefix length
     */
    private static int bprefix(final String left, final String right) {
        return Math.min(
            PhSuggestions.PREF,
            PhSuggestions.commonPrefix(
                PhSuggestions.compact(left),
                PhSuggestions.compact(right)
            )
        );
    }

    /**
     * Jaro similarity.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double jaro(final String left, final String right) {
        final double score;
        if (left.equals(right)) {
            score = PhSuggestions.TOP;
        } else if (left.isEmpty() || right.isEmpty()) {
            score = PhSuggestions.ZERO;
        } else {
            score = PhSuggestions.jaroMatched(left, right);
        }
        return score;
    }

    /**
     * Jaro similarity for non-empty different texts.
     * @param left Left text
     * @param right Right text
     * @return Score
     */
    private static double jaroMatched(final String left, final String right) {
        final PhSuggestions.Matches matches = PhSuggestions.Matches.matched(left, right);
        final double score;
        if (matches.count == 0) {
            score = PhSuggestions.ZERO;
        } else {
            score = (1.0D * matches.count / left.length()
                + 1.0D * matches.count / right.length()
                + (matches.count - matches.transpositions() / 2.0D) / matches.count)
                / 3.0D;
        }
        return score;
    }

    /**
     * Longest common prefix length.
     * @param left Left text
     * @param right Right text
     * @return Prefix length
     */
    private static int commonPrefix(final String left, final String right) {
        final int max = Math.min(left.length(), right.length());
        int pos = 0;
        while (pos < max && left.charAt(pos) == right.charAt(pos)) {
            ++pos;
        }
        return pos;
    }

    /**
     * Clamp score to similarity interval.
     * @param score Raw score
     * @return Clamped score
     */
    private static double clamp(final double score) {
        return Math.max(
            PhSuggestions.ZERO,
            Math.min(PhSuggestions.TOP, score)
        );
    }

    /**
     * Ranked suggestion.
     * @since 1.0
     */
    private static final class Ranked implements Comparable<PhSuggestions.Ranked> {

        /**
         * Candidate name.
         */
        private final String name;

        /**
         * Similarity score.
         */
        private final double score;

        /**
         * Ctor.
         * @param name Name
         * @param score Score
         */
        Ranked(final String name, final double score) {
            this.name = name;
            this.score = score;
        }

        @Override
        public int compareTo(final PhSuggestions.Ranked other) {
            int compared = Double.compare(other.score, this.score);
            if (compared == 0) {
                compared = this.name.compareTo(other.name);
            }
            return compared;
        }

        @Override
        public boolean equals(final Object obj) {
            final boolean equal;
            if (this == obj) {
                equal = true;
            } else if (obj instanceof PhSuggestions.Ranked) {
                final PhSuggestions.Ranked other = (PhSuggestions.Ranked) obj;
                equal = this.name.equals(other.name)
                    && Double.compare(this.score, other.score) == 0;
            } else {
                equal = false;
            }
            return equal;
        }

        @Override
        public int hashCode() {
            return Objects.hash(this.name, this.score);
        }

        /**
         * Make ranked suggestion.
         * @param origin Origin
         * @param candidate Candidate
         * @return Ranked suggestion
         */
        private static PhSuggestions.Ranked ranked(final String origin, final String candidate) {
            return new PhSuggestions.Ranked(
                candidate,
                PhSuggestions.score(origin, candidate)
            );
        }
    }

    /**
     * Jaro matches.
     * @since 1.0
     */
    private static final class Matches {

        /**
         * Left text.
         */
        private final String left;

        /**
         * Right text.
         */
        private final String right;

        /**
         * Left matched positions.
         */
        private final boolean[] lefts;

        /**
         * Right matched positions.
         */
        private final boolean[] rights;

        /**
         * Match count.
         */
        private final int count;

        /**
         * Ctor.
         * @param left Left text
         * @param right Right text
         * @param lefts Left matched positions
         * @param rights Right matched positions
         * @param count Match count
         * @checkstyle ParameterNumberCheck (10 lines)
         */
        @SuppressWarnings("PMD.ArrayIsStoredDirectly")
        Matches(
            final String left,
            final String right,
            final boolean[] lefts,
            final boolean[] rights,
            final int count
        ) {
            this.left = left;
            this.right = right;
            this.lefts = lefts;
            this.rights = rights;
            this.count = count;
        }

        /**
         * Find matches for two texts.
         * @param left Left text
         * @param right Right text
         * @return Matches
         */
        private static PhSuggestions.Matches matched(final String left, final String right) {
            final boolean[] lefts = new boolean[left.length()];
            final boolean[] rights = new boolean[right.length()];
            final int distance = Math.max(
                0,
                Math.max(left.length(), right.length()) / 2 - 1
            );
            int count = 0;
            for (int pos = 0; pos < left.length(); ++pos) {
                count += PhSuggestions.Matches.match(
                    left, right, lefts, rights, pos, distance
                );
            }
            return new PhSuggestions.Matches(left, right, lefts, rights, count);
        }

        /**
         * Match one left position.
         * @param left Left text
         * @param right Right text
         * @param lefts Left matched positions
         * @param rights Right matched positions
         * @param pos Left position
         * @param distance Match distance
         * @return Match count
         * @checkstyle ParameterNumberCheck (10 lines)
         */
        private static int match(
            final String left,
            final String right,
            final boolean[] lefts,
            final boolean[] rights,
            final int pos,
            final int distance
        ) {
            final int start = Math.max(0, pos - distance);
            final int end = Math.min(pos + distance + 1, right.length());
            int found = 0;
            for (int idx = start; idx < end; ++idx) {
                if (!rights[idx] && left.charAt(pos) == right.charAt(idx)) {
                    lefts[pos] = true;
                    rights[idx] = true;
                    found = 1;
                    break;
                }
            }
            return found;
        }

        /**
         * Count transpositions.
         * @return Transpositions
         */
        private int transpositions() {
            int trs = 0;
            int ridx = 0;
            for (int lidx = 0; lidx < this.left.length(); ++lidx) {
                if (this.lefts[lidx]) {
                    while (!this.rights[ridx]) {
                        ++ridx;
                    }
                    if (this.left.charAt(lidx) != this.right.charAt(ridx)) {
                        ++trs;
                    }
                    ++ridx;
                }
            }
            return trs;
        }
    }
}
