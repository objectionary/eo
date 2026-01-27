/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.aspects.RetryOnFailure;
import com.jcabi.log.Logger;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.Proxy;
import java.net.URL;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.cactoos.Input;
import org.cactoos.io.InputOf;
import org.cactoos.io.InputWithFallback;

/**
 * The simple HTTP Objectionary server.
 * <p>This class is supposed to be used together with {@link OyCached}.</p>
 *
 * @since 0.1
 */
final class OyRemote implements Objectionary {

    /**
     * The address template to program.
     */
    private final UrlOy program;

    /**
     * The address template to directory.
     */
    private final UrlOy directory;

    /**
     * Proxies list to use for HTTP if needed.
     */
    private final List<Proxy> proxies;

    /**
     * Constructor.
     * @param hash Commit hash
     * @param proxies Proxies to use
     */
    OyRemote(final CommitHash hash, final Proxy... proxies) {
        this.program = new UrlOy(
            "https://raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo",
            hash
        );
        this.directory = new UrlOy(
            "https://github.com/objectionary/home/tree/%s/objects/%s",
            hash
        );
        this.proxies = List.of(proxies);
    }

    @Override
    public String toString() {
        return String.format(
            "Program template: %s\nDirectory template: %s",
            this.program,
            this.directory
        );
    }

    @Override
    public Input get(final String name) throws MalformedURLException {
        final URL url = this.program.value(name);
        Logger.debug(
            this, "The object '%s' will be pulled from %s...",
            name, url
        );
        final Input remote;
        if (this.proxies.isEmpty()) {
            remote = new InputOf(url);
        } else {
            remote = () -> url.openConnection(this.proxies.get(0)).getInputStream();
        }
        return new InputWithFallback(
            remote,
            input -> {
                throw new IOException(
                    String.format(
                        "EO object '%s' is not found in this GitHub repository: https://github.com/objectionary/home by url: %s. This means that you either misspelled the name of it or simply referred to your own local object somewhere in your code as if it was an object of 'org.eolang' package. Check the sources and make sure you always use +alias meta when you refer to an object outside of 'org.eolang', even if this object belongs to your package.",
                        url,
                        name
                    ),
                    input
                );
            }
        );
    }

    @Override
    public boolean contains(final String name) throws IOException {
        final URL file = this.program.value(name);
        final URL dir = this.directory.value(name);
        return OyRemote.exists(file) || OyRemote.exists(dir);
    }

    @Override
    public boolean isDirectory(final String name) throws IOException {
        final URL url = this.directory.value(name);
        return OyRemote.exists(url);
    }

    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private static boolean exists(final URL url) throws IOException {
        final int code = ((HttpURLConnection) url.openConnection()).getResponseCode();
        return code >= HttpURLConnection.HTTP_OK && code < HttpURLConnection.HTTP_BAD_REQUEST;
    }

    /**
     * Objectionary URL template.
     *
     * <p>Assumes two placeholders in terms of
     * {@link String#format(String, Object...)}: 1st for version hash,
     * 2nd for program or directory name, for
     * <a href="https://raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo">programExample</a>
     * or
     * <a href="https://github.com/objectionary/home/tree/%s/objects/%s">directoryExample</a>.</p>
     *
     * @since 0.1.0
     */
    static final class UrlOy {

        /**
         * URL template.
         *
         * <p>Expects two placeholders in terms of
         * {@link String#format(String, Object...)}: 1st for hash,
         * 2nd for program or directory name, for
         * <a href="https://raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo">programExample</a>
         * or
         * <a href="https://github.com/objectionary/home/tree/%s/objects/%s">directoryExample</a>.</p>
         */
        private final String template;

        /**
         * Objects version hash.
         */
        private final CommitHash hash;

        /**
         * Ctor for testing.
         * @param template URL template
         * @param hash Commit hash
         */
        UrlOy(final String template, final String hash) {
            this(template, () -> hash);
        }

        /**
         * Ctor.
         * @param template URL template.
         * @param hash Objects version hash.
         */
        UrlOy(final String template, final CommitHash hash) {
            this.template = template;
            this.hash = hash;
        }

        /**
         * URL for the program or directory.
         * @param name Fully qualified EO program as specified by {@link Place} or directory name
         * @return URL
         * @throws MalformedURLException in case of incorrect URL
         */
        public URL value(final String name) throws MalformedURLException {
            return new URL(
                String.format(
                    this.template,
                    this.hash.value(),
                    name.replace(".", "/")
                )
            );
        }

        @Override
        public String toString() {
            return this.template;
        }
    }

}
