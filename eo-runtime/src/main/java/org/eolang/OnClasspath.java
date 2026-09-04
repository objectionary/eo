/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * The EO object names available on the classpath, collected by scanning the
 * {@code org/eolang/} package — a directory when running from classes, or an
 * entry inside a jar (#4520).
 * @since 0.74.0
 */
final class OnClasspath {

    /**
     * The root package that hosts the transpiled EO objects.
     */
    private static final String ROOT = "org/eolang/";

    /**
     * Logger.
     */
    private static final Logger LOGGER =
        Logger.getLogger(OnClasspath.class.getName());

    /**
     * The EO object names found on the classpath.
     */
    private final List<String> names;

    /**
     * Ctor.
     * @param loader The class loader to scan
     */
    OnClasspath(final ClassLoader loader) {
        this(OnClasspath.scan(loader));
    }

    /**
     * Ctor.
     * @param names The EO object names
     */
    OnClasspath(final List<String> names) {
        this.names = names;
    }

    /**
     * The EO object names found on the classpath.
     * @return The EO object names
     */
    List<String> names() {
        return this.names;
    }

    private static List<String> scan(final ClassLoader loader) {
        final List<String> found = new ArrayList<>(0);
        try {
            final Enumeration<URL> res = loader
                .getResources(OnClasspath.ROOT);
            while (res.hasMoreElements()) {
                final URL url = res.nextElement();
                if ("file".equals(url.getProtocol())) {
                    OnClasspath.tree(found, new File(url.toURI()));
                } else if ("jar".equals(url.getProtocol())) {
                    OnClasspath.jar(found, url);
                }
            }
        } catch (final IOException | URISyntaxException ex) {
            OnClasspath.LOGGER.log(
                Level.WARNING,
                "Cannot scan the classpath for EO object names, the list will be empty",
                ex
            );
        }
        return found;
    }

    private static void tree(final List<String> found, final File dir) {
        final File[] children = dir.listFiles();
        if (children != null) {
            for (final File child : children) {
                if (child.isDirectory()) {
                    OnClasspath.tree(found, child);
                } else {
                    OnClasspath.plus(
                        found, child.getPath().substring(dir.getPath().length() + 1)
                    );
                }
            }
        }
    }

    private static void jar(final List<String> found, final URL url) {
        try (JarFile jar = new JarFile(url.getPath().replace("file:", ""))) {
            final Enumeration<JarEntry> entries = jar.entries();
            while (entries.hasMoreElements()) {
                final String path = entries.nextElement().getName();
                if (path.startsWith(OnClasspath.ROOT)
                    && path.endsWith(".class")) {
                    OnClasspath.plus(
                        found,
                        path.replace('/', '.').substring(OnClasspath.ROOT.length())
                    );
                }
            }
        } catch (final IOException ex) {
            OnClasspath.LOGGER.log(
                Level.WARNING,
                String.format("Cannot read the jar '%s', its EO objects are skipped", url),
                ex
            );
        }
    }

    private static void plus(final List<String> found, final String path) {
        final String name = OnClasspath.toEo(path.replace(".class", ""));
        if (!name.isEmpty()) {
            found.add(name);
        }
    }

    private static String toEo(final String java) {
        final StringBuilder out = new StringBuilder(java.length());
        boolean valid = true;
        for (final String part : java.split("\\.", -1)) {
            final String segment = OnClasspath.segment(part);
            if (segment == null) {
                valid = false;
                break;
            }
            if (out.length() > 0) {
                out.append('.');
            }
            out.append(OnClasspath.dashes(segment));
        }
        String result = "";
        if (valid && !out.toString().endsWith("Test")) {
            result = out.toString();
        }
        return result;
    }

    private static String segment(final String part) {
        final String result;
        if (part.startsWith("EO_") && OnClasspath.clean(part)) {
            result = part.substring(3);
        } else if (part.startsWith("EO") && OnClasspath.clean(part)) {
            result = part.substring(2);
        } else {
            result = null;
        }
        return result;
    }

    private static boolean clean(final String part) {
        return !part.isEmpty() && !part.contains("$") && !part.contains("package-info");
    }

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
}
