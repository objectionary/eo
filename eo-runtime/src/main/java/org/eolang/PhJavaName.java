/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Java class name to EO object name conversion.
 * @since 1.0
 */
final class PhJavaName {

    /**
     * Java object prefix.
     */
    private static final String EOP = "EO";

    /**
     * Ctor.
     */
    private PhJavaName() {
    }

    /**
     * Original EO name from annotation.
     * @param xmir Annotation
     * @return EO name
     */
    static String original(final XmirObject xmir) {
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
    static String inPackage(final String parent, final String object) {
        final String pkg = PhJavaName.toObject(parent);
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
    static String parent(final String relative) {
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
     * Convert Java path to EO object name.
     * @param path Java resource path
     * @return EO object name
     */
    static String toObject(final String path) {
        final String object;
        if (path.isEmpty()) {
            object = "";
        } else {
            final List<String> parts = Arrays.asList(path.split("/"));
            if (parts.stream().allMatch(part -> part.startsWith(PhJavaName.EOP))) {
                object = parts.stream()
                    .map(part -> part.substring(PhJavaName.EOP.length()))
                    .map(PhJavaName::dashes)
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
            out.append(PhJavaName.dash(name, pos));
            pos += PhJavaName.skip(name, pos);
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
        if (PhJavaName.escaped(name, pos)) {
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
        if (PhJavaName.escaped(name, pos)) {
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
}
