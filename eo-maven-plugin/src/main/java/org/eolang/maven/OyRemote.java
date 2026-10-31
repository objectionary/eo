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
import java.time.Duration;
import java.util.concurrent.TimeUnit;
import org.cactoos.Input;
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
     * Deadline for connecting and waiting for response headers, per attempt.
     */
    private final Duration timeout;

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
     */
    OyRemote(final UrlOy program, final UrlOy directory, final MvnProxy... proxies) {
        this(program, directory, Duration.ofSeconds(30), proxies);
    }

    /**
     * Ctor with a deadline for testing.
     *
     * @param program Address template to program
     * @param directory Address template to directory
     * @param timeout Connection and request timeout
     * @param proxies Proxies to use
     * @checkstyle ConstructorsCodeFreeCheck (10 lines)
     */
    OyRemote(final UrlOy program, final UrlOy directory, final Duration timeout,
        final MvnProxy... proxies) {
        this.program = program;
        this.directory = directory;
        this.timeout = timeout;
        this.http = OyRemote.client(timeout, proxies);
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
        return () -> this.fetch(name, url);
    }

    @Override
    public boolean contains(final String name) throws IOException {
        return this.isProgram(name) || this.isDirectory(name);
    }

    @Override
    public boolean isDirectory(final String name) throws IOException {
        return this.exists(this.directory.value(name));
    }

    @Override
    public boolean isProgram(final String name) throws IOException {
        return this.exists(this.program.value(name));
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
        if (code == HttpURLConnection.HTTP_CLIENT_TIMEOUT || code == 429
            || code >= HttpURLConnection.HTTP_INTERNAL_ERROR && code < 600) {
            throw new IOException(
                String.format("Transient HTTP error %d for %s, will retry", code, url)
            );
        }
        return code >= HttpURLConnection.HTTP_OK && code < HttpURLConnection.HTTP_BAD_REQUEST;
    }

    @RetryOnFailure(delay = 1L, unit = TimeUnit.SECONDS)
    private InputStream fetch(final String name, final URL url) throws IOException {
        final HttpResponse<InputStream> response;
        try {
            response = this.send(url, HttpResponse.BodyHandlers.ofInputStream());
        } catch (final IOException ex) {
            throw new IOException(
                String.format("Failed to download EO object '%s' from %s", name, url),
                ex
            );
        }
        final int code = response.statusCode();
        final InputStream body = response.body();
        if (code == HttpURLConnection.HTTP_CLIENT_TIMEOUT || code == 429) {
            body.close();
            throw new IOException(
                String.format("Transient HTTP error %d for %s, will retry", code, url)
            );
        }
        if (code == HttpURLConnection.HTTP_NOT_FOUND) {
            body.close();
            throw new IOException(
                String.format(
                    "EO object '%s' is not found in this GitHub repository: https://github.com/objectionary/home by url: %s. This means that you either misspelled the name of it or simply referred to your own local object somewhere in your code as if it was an object of 'org.eolang' package. Check the sources and make sure you always use +alias meta when you refer to an object outside of 'org.eolang', even if this object belongs to your package.",
                    name,
                    url
                )
            );
        }
        if (code < HttpURLConnection.HTTP_OK || code >= HttpURLConnection.HTTP_BAD_REQUEST) {
            body.close();
            throw new IOException(
                String.format(
                    "Failed to download EO object '%s' from %s, HTTP error %d",
                    name, url, code
                )
            );
        }
        return body;
    }

    private <T> HttpResponse<T> send(
        final URL url, final HttpResponse.BodyHandler<T> handler
    ) throws IOException {
        try {
            return this.http.send(
                HttpRequest.newBuilder().uri(URI.create(url.toString()))
                    .timeout(this.timeout).GET().build(),
                handler
            );
        } catch (final InterruptedException ex) {
            Thread.currentThread().interrupt();
            throw new IOException(ex);
        }
    }

    private static HttpClient client(final Duration timeout, final MvnProxy... proxies) {
        final HttpClient.Builder builder = HttpClient.newBuilder()
            .connectTimeout(timeout)
            .followRedirects(HttpClient.Redirect.NORMAL);
        if (proxies.length > 0) {
            builder.proxy(new NonProxyHostsSelector(proxies[0]));
            proxies[0].credentials().ifPresent(builder::authenticator);
        }
        return builder.build();
    }
}
