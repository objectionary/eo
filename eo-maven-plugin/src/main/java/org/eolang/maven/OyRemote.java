/*
 * SPDX-FileCopyrightText: Copyright (c) 2016-2026 Objectionary.com
 * SPDX-License-Identifier: MIT
 */
package org.eolang.maven;

import com.jcabi.aspects.RetryOnFailure;
import com.jcabi.log.Logger;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.concurrent.TimeUnit;
import org.cactoos.Input;
import org.cactoos.io.InputWithFallback;
import org.cactoos.iterable.IterableOf;

/**
 * The simple HTTP Objectionary server.
 *
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
     * HTTP client, configured with proxy when provided.
     */
    private final HttpClient http;

    /**
     * Constructor.
     *
     * @param hash Commit hash
     * @param proxies Proxies to use
     */
    OyRemote(final CommitHash hash, final MvnProxy... proxies) {
        this(
            new UrlOy(
                "https://raw.githubusercontent.com/objectionary/home/%s/objects/%s.eo",
                hash
            ),
            new UrlOy(
                "https://github.com/objectionary/home/tree/%s/objects/%s",
                hash
            ),
            proxies
        );
    }

    /**
     * Ctor for testing.
     *
     * @param program Address template to program
     * @param directory Address template to directory
     * @param proxies Proxies to use
     * @checkstyle ConstructorsCodeFreeCheck (10 lines)
     */
    OyRemote(final UrlOy program, final UrlOy directory, final MvnProxy... proxies) {
        this.program = program;
        this.directory = directory;
        this.http = OyRemote.client(proxies);
    }

    @Override
    public String toString() {
        return String.format(
            "Program template: %s%nDirectory template: %s",
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
        return new InputWithFallback(
            () -> this.fetch(url),
            input -> {
                throw new IOException(
                    String.format(
                        "EO object '%s' is not found in this GitHub repository: https://github.com/objectionary/home by url: %s. This means that you either misspelled the name of it or simply referred to your own local object somewhere in your code as if it was an object of 'org.eolang' package. Check the sources and make sure you always use +alias meta when you refer to an object outside of 'org.eolang', even if this object belongs to your package.",
                        name,
                        url
                    ),
                    input
                );
            }
        );
    }

    @Override
    public boolean contains(final String name) throws IOException {
        return this.exists(this.program.value(name)) || this.exists(this.directory.value(name));
    }

    @Override
    public boolean isDirectory(final String name) throws IOException {
        return this.exists(this.directory.value(name));
    }

    @Override
    public Iterable<String> children(final String pkg) {
        Logger.debug(
            this,
            "The remote objectionary can't list the package '%s' cheaply, so no objects are returned; enumeration is done through the objects index instead",
            pkg
        );
        return new IterableOf<>();
    }

    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private boolean exists(final URL url) throws IOException {
        final int code = this.send(url, HttpResponse.BodyHandlers.discarding()).statusCode();
        if (code == HttpURLConnection.HTTP_CLIENT_TIMEOUT || code == 429) {
            throw new IOException(
                String.format("Transient HTTP error %d for %s, will retry", code, url)
            );
        }
        return code >= HttpURLConnection.HTTP_OK && code < HttpURLConnection.HTTP_BAD_REQUEST;
    }

    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private InputStream fetch(final URL url) throws IOException {
        final HttpResponse<InputStream> response = this.send(
            url, HttpResponse.BodyHandlers.ofInputStream()
        );
        final int code = response.statusCode();
        if (code == HttpURLConnection.HTTP_CLIENT_TIMEOUT || code == 429) {
            throw new IOException(
                String.format("Transient HTTP error %d for %s, will retry", code, url)
            );
        }
        if (code < HttpURLConnection.HTTP_OK || code >= HttpURLConnection.HTTP_BAD_REQUEST) {
            throw new IOException(
                String.format("HTTP error %d for %s", code, url)
            );
        }
        return response.body();
    }

    private <T> HttpResponse<T> send(
        final URL url, final HttpResponse.BodyHandler<T> handler
    ) throws IOException {
        try {
            return this.http.send(
                HttpRequest.newBuilder().uri(URI.create(url.toString())).GET().build(),
                handler
            );
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IOException(ex);
        }
    }

    private static HttpClient client(final MvnProxy... proxies) {
        final HttpClient.Builder builder = HttpClient.newBuilder()
            .followRedirects(HttpClient.Redirect.NORMAL);
        if (proxies.length > 0) {
            builder.proxy(new NonProxyHostsSelector(proxies[0]));
            proxies[0].credentials().ifPresent(builder::authenticator);
        }
        return builder.build();
    }
}
